{-
   File      :  Sprite.hs
   Abstraction for common behaviors of elements needing to be rendered
   on screen.
-}

module Sprite
(
   Sprite(..),
   Movement(..),
   mkSprite,
   render,
   moveSprite,
   update,
   spriteInList,
   spriteOffScreen,
   detectCollision
) where

import Graphics.Gloss.Data.Point
import Graphics.Gloss
import GameConstants

data Sprite = Sprite {
                loc::Point,
                dim::Size,
                move::Movement,
                elapsedTime::Float
              }
  deriving (Show, Eq)

data Movement = MoveUp
              | MoveDown
              | MoveLeft
              | MoveRight
              | NoMove
              | AngledMove Float Float
  deriving (Show, Eq)

{- Render a sprite -}
render :: Sprite -> IO Picture -> IO Picture
render sprite spritePic = do
    pic <- spritePic
    return $ translate (fst $ loc sprite) (snd $ loc sprite) pic

{- Make a sprite -}
mkSprite :: Float -> Float -> Int -> Int -> Sprite
mkSprite lX lY dX dY = Sprite (lX, lY) (dX, dY) NoMove 0

{- Move a sprite -}
moveSprite :: Movement -> Sprite -> Sprite
moveSprite newMove s = s { move = newMove }

{- Update a sprite's position nad reset move if necessary -}
update :: Float -> Float -> Sprite -> Sprite
update deltaTime speed sprite@(Sprite (x,y) dim sMove sElapsed) =
  let elapsedTime = sElapsed + deltaTime
      in
      if (elapsedTime >= deltaTime)
      then (case sMove of
              m@(AngledMove dX dY) -> (Sprite (x + dX, y + dY) dim m 0.0)
              MoveLeft  -> (Sprite (x - speed, y) dim NoMove 0.0)
              MoveRight -> (Sprite (x + speed, y) dim NoMove 0.0)
              MoveUp    -> (Sprite (x, y + speed) dim NoMove 0.0)
              MoveDown  -> (Sprite (x, y - speed) dim NoMove 0.0)
              NoMove    -> sprite
           )
      else (Sprite (x, y) dim sMove elapsedTime)

{- Determine if a sprite is in a list of sprites, using location as a fairly
reliable identifier -}
spriteInList :: Sprite -> [Sprite] -> Bool
spriteInList s sList = elem (loc s) (loc <$> sList)

{- Detect a collision between two sprites, using the first sprite's location
as the point and the second sprite as the box -}
detectCollision :: Sprite -> Sprite -> Bool
detectCollision subject other = pointInBox subLoc otherBotRight otherTopLeft
  where
    subLoc = loc subject
    xOffset = (*) 0.5 $ realToFrac . fst $ Sprite.dim other
    yOffset = (*) 0.5 $ realToFrac . snd $ Sprite.dim other
    otherBotRight = (,)
                      ((+) (fst $ loc other) xOffset)
                      ((-) (snd $ loc other) yOffset)
    otherTopLeft = (,)
                      ((-) (fst $ loc other) xOffset)
                      ((+) (snd $ loc other) yOffset)

{- Determine if a sprite has gone offscreen, with some leeway for enemies that
are moving along a curve -}
spriteOffScreen :: Sprite -> Bool
spriteOffScreen s
  | xPos > (maxXPos + leeway) || xPos < (minXPos - leeway) = True
  | yPos > (maxYPos + leeway) || yPos < (minYPos - leeway) = True
  | otherwise = False
  where
    xPos = fst $ loc s
    yPos = snd $ loc s
    leeway = 50
