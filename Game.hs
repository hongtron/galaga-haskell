{-
   File      :  Game.hs
   Contains functions used to progress the game.
-}

module Game
(
  GameState(..),
  gameLoop,
  initState,
  gLevel
) where

import Sprite
import Ship
import Enemy
import Bullet
import HighScores
import Graphics.Gloss
import Data.Maybe
import Data.List
import Explosion
import System.Random

data GameState    = MainMenu
                  | Game { gShip::Ship,
                           gBullets::[Bullet],
                           gEnemies::[Enemy],
                           gExp::[Explosion],
                           gElapsedTime::Float,
                           gPlayerLives::Int,
                           gOpenSpots::[Point],
                           gPlayerScore::Int }
                  | HighScores Bool
                  | GameOver Int

{- Initial gameplay state -}
initState :: GameState
initState = Game initShip [] mkEnemies [] 0.0 3 [] 0

{- Progress to harder levels as more enemies are killed -}
gLevel :: GameState -> Float
gLevel (Game _ _ _ _ _ _ _ score) = realToFrac $ score `div` 18
gLevel g = -1

{- Calculate probability of enemy leaving formation based on current level -}
rogueProb :: Float -> Float
rogueProb level = 0.0004 + (0.0001 * level)

{- Calculate probability of rogue enemy firing based on current level -}
fireProb :: Float -> Float
fireProb level = 0.02 + (0.01 * level)

{- Determine whether enemies will go rogue or fire based on calculated probs -}
determineEnemyBehavior :: [Float] -> Float -> ([Bool], [Bool])
determineEnemyBehavior determinants level = (firingStatuses, rogueStatuses)
  where
    firingStatuses = map (\fd -> fd < fireProb level) determinants
    rogueStatuses = map (\fd -> fd < rogueProb level) determinants

{- Game Over if player dies on their last life -}
gameLoop :: Float -> GameState -> IO GameState
gameLoop deltaTime (Game _ _ _ _ _ (-1) _ score) = do
  playerGotHighScore <- evaluateScore score
  if (playerGotHighScore)
  then (return $ HighScores True)
  else (return $ GameOver score)

{- Regenerate enemies at the start of each new level -}
gameLoop deltaTime g@(Game ship bullets [] exps et lives spots score) =
  return (Game ship bullets mkEnemies exps 0.0 lives spots score)

{- Main gameplay loop -}
gameLoop deltaTime g@(Game ship bullets enemies exps et lives spots score) = do
  let g' = updateSprites g deltaTime
  let (survivingEnemies, killedEnemies) = evaluateEnemyStates g'
  let survivingBullets = evaluateBulletStates g'
  let (ship', lives', shipExplosion) = evaluatePlayerState g'
  let explosions' = newExplosions g' shipExplosion killedEnemies

  {- Random number used to determine whether enemy will go rogue or fire -}
  eBehaviorDeterminants <- mapM (\e -> randomRIO (0.0,1.0) :: IO Float)
                                survivingEnemies

  {- Pick path to traverse if the enemy does go rogue -}
  pathsForRogues <- mapM (\e -> randomRIO (0,2) :: IO Int) survivingEnemies

  let (firingStatuses, rogueStatuses) =
        determineEnemyBehavior eBehaviorDeterminants (gLevel g)
  let newBullets = catMaybes $ zipWith enemyFire survivingEnemies firingStatuses
  let (updatedFormation, newlyOpenSpots) =
        deployNewRogues survivingEnemies rogueStatuses pathsForRogues

  {- Player scores a point for every enemy killed -}
  let score' = score + (length enemies) - (length updatedFormation)

  return $ Game
            ship'
            (survivingBullets ++ newBullets)
            updatedFormation
            explosions'
            (et + deltaTime)
            lives'
            ((gOpenSpots g') ++ newlyOpenSpots)
            score'

{- Game loop for static screens -}
gameLoop deltaTime g = return g

{- Move or remove sprites based on their current move -}
updateSprites :: GameState -> Float -> GameState
updateSprites g@(Game {}) dt =
  g {
    gShip = updateShip dt (gShip g),
    gEnemies = fst updatedEnemiesAndSpots,
    gOpenSpots = snd updatedEnemiesAndSpots,
    gBullets = catMaybes $ updateBullet dt <$> (gBullets g),
    gExp = updateExplosion dt <$> (gExp g)
  }
    where
      updatedEnemiesAndSpots =
        updateEnemiesAndSpots dt (gElapsedTime g) (gEnemies g) (gOpenSpots g)

{- Updating enemies and spots are closely related (see documentation for
individual functions) -}
updateEnemiesAndSpots ::
  Float -> Float -> [Enemy] -> [Point] -> ([Enemy], [Point])
updateEnemiesAndSpots dt et enemies spots = (updatedEnemies, updatedSpots)
  where
    formationMove = formationDirection et
    updatedEnemySprites = updateEnemy dt formationMove <$> enemies
    (offscreenEnemies, onscreenEnemies) =
      partition (spriteOffScreen . enemySprite) updatedEnemySprites
    updatedEnemies =
      (onscreenEnemies ++) $ zipWith repositionEnemy offscreenEnemies spots
    updatedSpots =
      drop (length offscreenEnemies) (updateOpenSpot formationMove <$> spots)

{- Separate surviving enemies from ones that have been killed -}
evaluateEnemyStates :: GameState -> ([Enemy], [Enemy])
evaluateEnemyStates g = (survivingEnemies, killedEnemies)
  where
    shipBullets = fst $ partitionedBullets (gBullets g)
    killedEnemies =
      snd $ partition isAlive $ killEnemy <$> shipBullets <*> (gEnemies g)
    survivingEnemies =
      filter (\e -> not $ enemyInList e killedEnemies) (gEnemies g)

{- Separate surviving bullets from ones that have collided with targets -}
evaluateBulletStates :: GameState -> [Bullet]
evaluateBulletStates g = checkShipBullets ++ checkEnemyBullets
  where
    (sBullets, eBullets) = partitionedBullets (gBullets g)
    {- Ship bullets are destroyed on collision with an enemy -}
    sbDangers = enemySprite <$> (gEnemies g)
    {- Enemy bullets are destroyed on collision with the ship -}
    ebDangers = (shipSprite $ gShip g):[]
    detectBulletDeath dangers b =
      not . or $ (detectCollision . bulletSprite) b <$> dangers
    checkShipBullets = filter (detectBulletDeath sbDangers) sBullets
    checkEnemyBullets = filter (detectBulletDeath ebDangers) eBullets

{- Determine if the player died; create explosion and decrement life count
if necessary -}
evaluatePlayerState :: GameState -> (Ship, Int, Maybe Explosion)
evaluatePlayerState g@(Game ship bullets enemies _ _ lives _ _) =
  if (detectShipDeath)
  then (initShip, lives - 1, Just $ explodeShip ship)
  else (ship, lives, Nothing)
  where
    {- Ships are vulnerable to both enemy bullets and enemy ships -}
    sDangers = (bulletSprite <$> bullets) ++ (enemySprite <$> enemies)
    detectShipDeath = or $ detectCollision <$> sDangers <*> (shipSprite ship):[]

{- Filter out completed explosions, add new enemy explosions, and explode
the player if they died -}
newExplosions :: GameState -> Maybe Explosion -> [Enemy] -> [Explosion]
newExplosions g sExp kEnemies =
  (maybeToList sExp ++) $ filter stillExploding $ (gExp g) ++ newEnemyExplosions
  where newEnemyExplosions = (explodeEnemy <$> kEnemies)
