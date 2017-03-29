{-
   File      :  GameConstants.hs
   Represents the constants values that will not change through out the
   execution of the game.
-}

module GameConstants
(
    screenSize,
    screenWidth,
    screenHeight,
    Size,
    fps,
    deltaTime,
    maxXPos,
    minXPos,
    maxYPos,
    minYPos,
    numHighScores,
    scoreFile
) where

import Graphics.Gloss

type Size = (Int,Int)

{- Size of the screen -}
screenSize :: Size
screenSize = (800,600)

{- Max value for an x coord -}
maxXPos :: Float
maxXPos = realToFrac (fst screenSize) * 0.5

{- Min value for an x coord -}
minXPos :: Float
minXPos = -1 * maxXPos

{- Max value for a y coord -}
maxYPos :: Float
maxYPos = realToFrac (snd screenSize) * 0.5

{- Min value for a y coord -}
minYPos :: Float
minYPos = -1 * maxYPos

screenWidth :: Int
screenWidth = fst screenSize

screenHeight :: Int
screenHeight = snd screenSize

{- Duration of a frame -}
deltaTime :: Float
deltaTime = (/) 1.0 $ realToFrac fps

{- Frame rate -}
fps:: Int
fps = 60

{- Number of high scores to track -}
numHighScores :: Int
numHighScores = 10

{- File to read/write scores from/to -}
scoreFile :: String
scoreFile = "high_scores.txt"
