{-
   File      :  Explosion.hs
   Encapsulate behaviors of explosion animation.
-}

module Explosion
(
  Explosion(..),
  ExplosionState(..),
  updateExplosion,
  renderExplosion,
  explodeEnemy,
  explodeShip,
  explosionFrames,
  stillExploding
) where

import Graphics.Gloss
import GameConstants
import Ship
import Enemy
import Sprite

data Explosion = Explosion { expState::ExplosionState, expSprite::Sprite }
  deriving (Show)

{- Define stages of an explosion -}
data ExplosionState = Exploding1
                    | Exploding2
                    | Exploding3
                    | Exploding4
                    | Exploding5
                    | Gone
  deriving (Show, Eq)

{- Render an explosion -}
renderExplosion :: Explosion -> IO Picture
renderExplosion exp@(Explosion state sprite) = do
  render sprite $ explosionFrames state

{- List of images for each explosion state -}
explosionFrames :: ExplosionState -> IO Picture
explosionFrames state
  | state == Exploding1 = loadBMP "images/models/explosion1.bmp"
  | state == Exploding2 || state == Exploding4 =
    loadBMP "images/models/explosion2.bmp"
  | state == Exploding3 || state == Exploding5 =
    loadBMP "images/models/explosion3.bmp"

{- Advance explosion state until it's over -}
updateExplosion :: Float -> Explosion -> Explosion
updateExplosion deltaTime e@(Explosion state sprite)
  | hasFramePassed && state == Exploding1 = (Explosion Exploding2 sprite)
  | hasFramePassed && state == Exploding2 = (Explosion Exploding3 sprite)
  | hasFramePassed && state == Exploding3 = (Explosion Exploding4 sprite)
  | hasFramePassed && state == Exploding4 = (Explosion Exploding5 sprite)
  | hasFramePassed && state == Exploding5 = (Explosion Gone sprite)
  | otherwise = e
  where
    elapsedTime = (Sprite.elapsedTime sprite) + deltaTime
    hasFramePassed = elapsedTime >= deltaTime

{- Returns True if the explosion is not yet over, False otherwise -}
stillExploding :: Explosion -> Bool
stillExploding (Explosion state _)
  | state /= Gone = True
  | otherwise = False

{- Replace an enemy with an explosion -}
explodeEnemy :: Enemy -> Explosion
explodeEnemy (Enemy _ _ _ eSprite) =
  Explosion Exploding1 $ mkSprite (fst $ loc eSprite) (snd $ loc eSprite) 35 35

{- Replace a ship with an explosion -}
explodeShip :: Ship -> Explosion
explodeShip (Ship sSprite) =
  Explosion Exploding1 $ mkSprite (fst $ loc sSprite) (snd $ loc sSprite) 35 35
