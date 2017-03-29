{-
   File      :  Bullet.hs
   Encapsulate behaviors of ship and enemy bullets.
-}

module Bullet
(
   Bullet(..),
   BulletType(..),
   mkShipBullet,
   mkEnemyBullet,
   moveBullet,
   renderBullet,
   updateBullet,
   enemyFire,
   partitionedBullets,
   killEnemy
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Data.List
import GameConstants
import Sprite
import Ship
import Enemy

data Bullet = Bullet { bType::BulletType, bulletSprite::Sprite }
  deriving (Show)

data BulletType = ShipBullet | EnemyBullet
  deriving (Show, Eq)

{- Render a bullet's sprite -}
renderBullet :: Bullet -> IO Picture
renderBullet bullet@(Bullet bt sprite) = do
    render sprite $ bulletPic bt

{- Choose appropriate image based on bullet type -}
bulletPic :: BulletType -> IO Picture
bulletPic bt
  | bt == ShipBullet = loadBMP "images/models/ship_bullet.bmp"
  | bt == EnemyBullet = loadBMP "images/models/enemy_bullet.bmp"

{- Create a bullet fired by the ship that moves straight up, and position it
above the ship -}
mkShipBullet :: Ship -> Bullet
mkShipBullet ship@(Ship sSprite) =
  moveBullet MoveUp $ Bullet ShipBullet $ mkSprite (fst $ loc sSprite)
                                            ((+) 20 . snd $ loc sSprite) 10 17

{- Create a bullet fired by an enemy, and position it below the enemy.
The previous location of the enemy is used to calculate a trajectory for the
bullet based on the movement of the enemy that fired it. -}
mkEnemyBullet :: Enemy -> Maybe Bullet
mkEnemyBullet enemy
  {- Don't fire bullets on horizontal or almost-horizontal trajectories -}
  | deltaY < (-4) = Just $ moveBullet (AngledMove deltaX deltaY) $
      Bullet EnemyBullet $
        mkSprite (fst $ loc eSprite) ((snd $ loc eSprite) - 20) 10 17
  | otherwise = Nothing
  where
    eSprite = enemySprite enemy
    rawDeltaX = (fst $ loc eSprite) - (fst . lastLoc $ rInfo enemy)
    rawDeltaY = (snd $ loc eSprite) - (snd . lastLoc $ rInfo enemy)
    {- Normalize the delta to ensure that bullets at different angles travel
    at uniform speed; Multiply the normalized vector to make the speed of the
    bullet configurable -}
    normalizedDelta = mulSV bulletSpeed $ normalizeV (rawDeltaX, rawDeltaY)
    deltaX = fst normalizedDelta
    deltaY = snd normalizedDelta

{- Move a bullet's sprite -}
moveBullet :: Movement -> Bullet -> Bullet
moveBullet newMove (Bullet bt sprite) = Bullet bt $ moveSprite newMove sprite

{- Update a bullet's location; remove it if it's gone off the screen -}
updateBullet :: Float -> Bullet -> Maybe Bullet
updateBullet deltaTime (Bullet bt sprite)
  | spriteOffScreen sprite = Nothing
  | bt == ShipBullet = Just $ Bullet bt $ moveSprite MoveUp $
      Sprite.update deltaTime bulletSpeed sprite
  | bt == EnemyBullet = Just $ Bullet bt $ moveSprite (move sprite) $
      Sprite.update deltaTime bulletSpeed sprite

{- Maybe return a bullet, but only if the enemy is supposed to fire -}
enemyFire :: Enemy -> Bool -> Maybe Bullet
enemyFire e shouldFire
  | shouldFire && (isRogue $ rInfo e) = mkEnemyBullet e
  | otherwise = Nothing

{- Set Enemy's isAlive to False if a collision is detected -}
killEnemy :: Bullet -> Enemy -> Enemy
killEnemy bullet enemy@(Enemy color alive rogue eSprite)
  | detectCollision (bulletSprite bullet) (enemySprite enemy) =
      Enemy color False rogue eSprite
  | otherwise = enemy

{- Group bullets by type (Enemy or Ship) -}
partitionedBullets :: [Bullet] -> ([Bullet], [Bullet])
partitionedBullets bullets = partition (\b -> bType b == ShipBullet) bullets

{- Define speed of bullet's movements -}
bulletSpeed :: Float
bulletSpeed = 7.0
