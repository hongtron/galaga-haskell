{-
   File      :  Enemy.hs
   Encapsulate behaviors of an enemy ship.
-}

module Enemy
(
   Enemy(..),
   RogueInfo(..),
   mkEnemies,
   moveEnemy,
   renderEnemy,
   updateEnemy,
   updateOpenSpot,
   goRogue,
   deployNewRogues,
   repositionEnemy,
   enemyInList,
   formationDirection
) where

import Graphics.Gloss
import GameConstants
import Sprite
import System.Random
import Data.Maybe

data Enemy = Enemy {
              enemyColor::EColor,
              isAlive::Bool,
              rInfo::RogueInfo,
              enemySprite::Sprite
            }
  deriving (Show, Eq)

data EColor = RedE | BlueE
  deriving (Show, Eq)

{- Holds info to determine behavior of enemies who have left formation
    isRogue - has the enemy left formation?
    curveStart - store the starting location of rogue's trajectory
    pathIndex - which path is the rogue on?
    lastLoc - rogue's location in the last frame; used to calculate trajectory
      of bullets
-}
data RogueInfo = RogueInfo {
                  isRogue::Bool,
                  curveStart::Point,
                  pathIndex::Int,
                  lastLoc::Point
                }
  deriving (Show, Eq)

{- Rogue info for enemies in formation (i.e. not rogue) -}
notRogue :: RogueInfo
notRogue = RogueInfo False (0.0, 0.0) (-1) (0.0, 0.0)

{- Render the enemy's sprite -}
renderEnemy :: Enemy -> IO Picture
renderEnemy enemy@(Enemy color _ _ sprite) = render sprite $ enemyPic color

{- Choose appropriate image -}
enemyPic :: EColor -> IO Picture
enemyPic RedE = loadBMP "images/models/red_enemy.bmp"
enemyPic BlueE = loadBMP "images/models/blue_enemy.bmp"

{- Create list of enemies in formation -}
mkEnemies :: [Enemy]
mkEnemies = zipWith
  (\loc color -> Enemy color True notRogue $ mkSprite (fst loc) (snd loc) 30 30)
  enemyStartingLocs $ cycle [RedE, BlueE]

{- Generate starting positions each enemy in formation -}
enemyStartingLocs :: [(Float, Float)]
enemyStartingLocs = [(x, y) | x <- [0, 50..250], y <- [250, 200..150]]

{- Determine which way the formation should move based on elapsed time -}
formationDirection :: Float -> Movement
formationDirection et
  | (truncate et) `mod` 4 == 0 = MoveLeft
  | (truncate et) `mod` 4 == 1 = NoMove
  | (truncate et) `mod` 4 == 2 = MoveRight
  | (truncate et) `mod` 4 == 3 = NoMove

{- Move the enemy's sprite -}
moveEnemy :: Movement -> Enemy -> Enemy
moveEnemy newMove e@(Enemy {enemySprite = sprite}) =
  e { enemySprite = moveSprite newMove sprite }

{- Move enemy to a new position; used if they leave the screen -}
repositionEnemy :: Enemy -> Point -> Enemy
repositionEnemy e@(Enemy {enemySprite = sprite}) pos =
  e { rInfo = notRogue, enemySprite = mkSprite (fst pos) (snd pos) 30 30 }

{- Populate RogueInfo depending on whether enemy goes rogue -}
goRogue :: Enemy -> Bool -> Int -> (Enemy, Maybe Point)
goRogue e doIt pathI
  | notAlreadyRogue && doIt = (e { rInfo = newRogueInfo }, Just (loc sprite))
  | otherwise = (e, Nothing)
  where
    sprite = enemySprite e
    notAlreadyRogue = not (isRogue $ rInfo e)
    newRogueInfo = RogueInfo True (loc sprite) pathI (loc sprite)

{- Update the enemy's sprite and/or rogue info -}
updateEnemy :: Float -> Movement -> Enemy -> Enemy
updateEnemy deltaTime move e@(Enemy {enemySprite = sprite})
  | isRogue $ rInfo e = updateRogue e
  | otherwise = moveEnemy move $ e {
                                  enemySprite =
                                    Sprite.update deltaTime enemySpeed sprite
                                   }
  where
    elapsedTime = (Sprite.elapsedTime sprite) + deltaTime

{- Return enemies with updated rogue info and newly vacated spots -}
deployNewRogues :: [Enemy] -> [Bool] -> [Int] -> ([Enemy], [Point])
deployNewRogues formation rStatuses rPaths = (formation', newSpots)
  where
    (formation', potentialSpots) =
      unzip $ zipWith3 goRogue formation rStatuses rPaths
    newSpots = catMaybes potentialSpots

{- Calculate location based on rogue's path, and store previous location to
be used to calculate bullet trajectory -}
updateRogue :: Enemy -> Enemy
updateRogue
  (Enemy
    color
    alive
    ri@(RogueInfo { lastLoc = _ })
    sprite@(Sprite loc dim move et)
  ) = Enemy color alive ri $ Sprite (newX, newY) dim NoMove (et + 0.003)
  where
    newX = enemyPathXPos (et) (fst $ lastLoc ri) (pathIndex ri)
    newY = enemyPathYPos (et) (snd $ lastLoc ri) (pathIndex ri)
    newRI = ri { lastLoc = loc }

{- Enemies must return to formation if they make it past the bottom of the
screen; update the array of open spots in the formation -}
updateOpenSpot :: Movement -> Point -> Point
updateOpenSpot move p
  | move == NoMove = p
  | move == MoveLeft = ((fst p - enemySpeed), snd p)
  | move == MoveRight = ((fst p + enemySpeed), snd p)

{- Determine if an enemy is in a list of enemies -}
enemyInList :: Enemy -> [Enemy] -> Bool
enemyInList e eList = spriteInList (enemySprite e) (enemySprite <$> eList)

{- Define speed of enemies' movements -}
enemySpeed :: Float
enemySpeed = 4.0

{- Bezier curve functions

Note: All paths start at (0,0) so that the values along the path can be
treated as offsets for the starting point of the curve. As such, some
terms would always evaluate to 0, and are therefore omitted from these
calculations. -}

{- Calculate the x coordinate of an enemy moving along as Bezier curve -}
enemyPathXPos :: Float -> Float -> Int -> Float
enemyPathXPos t xPos curveIndex =
  (+) xPos $ (3*uu*t*(curveXVals !! 0)) + (3*u*tt*(curveXVals !! 1))
  where
    u = 1 - t
    tt = t*t
    uu = u*u
    uuu = uu*u
    ttt = tt*t
    curveXVals = xValsForEnemyPaths !! curveIndex

{- Calculate the y coordinate of an enemy moving along as Bezier curve -}
enemyPathYPos :: Float -> Float -> Int -> Float
enemyPathYPos t yPos curveIndex =
  (+) yPos $ (3*uu*t*(curveYVals !! 0)) +
                (3*u*tt*(curveYVals !! 1)) +
                (ttt*(curveYVals !! 2))
  where
    u = 1 - t
    tt = t*t
    uu = u*u
    uuu = uu*u
    ttt = tt*t
    curveYVals = yValsForEnemyPaths !! curveIndex

{- Px values for an assortment of pre-determined enemy paths -}
xValsForEnemyPaths :: [[Float]]
xValsForEnemyPaths = [
    [(-400), 400],
    [(-300), (-150)],
    [300, (-270)]
  ]

{- Py values for an assortment of pre-determined enemy paths -}
yValsForEnemyPaths :: [[Float]]
yValsForEnemyPaths = [
    [(-500), 150, (-300)],
    [(-780), 370, (-450)],
    [(-400), (-250), (-450)]
  ]
