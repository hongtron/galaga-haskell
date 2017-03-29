{-
   File      :  Ship.hs
   Encapsulate behaviors of player's ship.
-}

module Ship
(
   Ship(..),
   mkShip,
   initShip,
   playerLives,
   moveShip,
   renderShip,
   updateShip
) where

import Graphics.Gloss
import GameConstants
import Sprite

data Ship = Ship { shipSprite::Sprite }

{- Render a ship's sprite -}
renderShip :: Ship -> IO Picture
renderShip ship@(Ship sprite) = do
    render sprite shipPic

{- Ship image -}
shipPic :: IO Picture
shipPic = loadBMP "images/models/ship.bmp"

{- Make a ship -}
mkShip :: Float -> Float -> Ship
mkShip xPos yPos = Ship $ mkSprite xPos yPos 30 30

{- Make a ship in the starting position -}
initShip :: Ship
initShip = mkShip 0.0 (-200.0)

{- Ship images to represent the player's remaining lives -}
playerLives :: [Ship]
playerLives =
  [mkShip (-340) (-250)] ++ [mkShip (-300) (-250)] ++ [mkShip (-260) (-250)]

{- Move a ship's sprite -}
moveShip :: Movement -> Ship -> Ship
moveShip newMove (Ship sprite) = Ship $ moveSprite newMove sprite

{- Update a ship's sprite -}
updateShip :: Float -> Ship -> Ship
updateShip deltaTime (Ship sprite) =
  Ship (Sprite.update deltaTime shipSpeed sprite)

{- Define speed of ship's movements -}
shipSpeed :: Float
shipSpeed = 10.0
