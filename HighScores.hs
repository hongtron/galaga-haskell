{-
   File      :  HighScores.hs
   Functionality to track and persist high scores.
-}

module HighScores
(
  getScores,
  evaluateScore
) where

import System.IO
import GameConstants
import Text.Read
import Data.List
import Data.Maybe
import Control.DeepSeq

{- Return an IO list of high scores read from high_scores.txt -}
getScores :: IO [Int]
getScores = do
  fileContents <- withFile scoreFile ReadMode (\handle -> readScoreFile handle)
  let scores = readMaybe <$> lines fileContents :: [Maybe Int]
  return $ validateScores scores

{- Get the contents of the score file, forcing evaluation of contents so
we don't run into locking issues later caused by lazy evaluation -}
readScoreFile :: Handle -> IO String
readScoreFile handle = do
    contents <- hGetContents handle
    return $!! contents

{- Only return valid int scores, and zero pad to number of high scores -}
validateScores :: [Maybe Int] -> [Int]
validateScores scoresFromFile =
  take numHighScores $ sortedValidScores ++ repeat 0
  where
    sortedValidScores = reverse . sort $ catMaybes scoresFromFile

{- Convenience function -}
positionToBeat :: Int
positionToBeat = numHighScores - 1

{- If given a new high score, write to file and return True. Otherwise,
simply return False -}
evaluateScore :: Int -> IO Bool
evaluateScore newScore = do
  currentHighScores <- getScores
  if (currentHighScores !! positionToBeat < newScore)
  then (addNewScore currentHighScores newScore)
  else (return False)

{- Helper to handle writing new high score to file and returning True -}
addNewScore :: [Int] -> Int -> IO Bool
addNewScore currentHighScores score = do
  let newHighScores =
        reverse . sort $ score:[] ++ (take positionToBeat currentHighScores)
  writeFile scoreFile $ unlines $ show <$> newHighScores
  return True
