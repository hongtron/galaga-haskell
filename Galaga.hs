{-
   File      :  Galaga.hs
   Represents the main starting point for the game
-}
module Main where

import Game
import HighScores
import GameConstants
import Graphics.Gloss.Interface.IO.Game
import Ship
import Enemy
import Bullet
import Explosion
import Sprite

window :: Display
window = InWindow "Galaga" screenSize (10,10)

main :: IO ()
main = playIO window black fps MainMenu Main.render eventHandler gameLoop

{- The render function takes a gamestate and renders it to the screen -}
render :: GameState -> IO Picture

render MainMenu = do
  welcome <- renderMessage 0.0 100.0 $ "Welcome to Galaga!"
  play <- renderMessage 0.0 0.0 $ "'p' play"
  highScores <- renderMessage 0.0 (-100.0) $ "'h' to see high scores"
  exit <- renderMessage 0.0 (-200.0) $ "'esc' to exit"
  let menuElements = welcome:[] ++ play:[] ++ highScores:[] ++ exit:[]
  return $ Pictures menuElements

render g@(Game ship bullets enemies exps _ lives _ score) = do
  enemies' <- mapM renderEnemy enemies
  ship' <- mapM renderShip [ship]
  lifeSprites <- mapM renderShip $ take lives playerLives
  bullets' <- mapM renderBullet bullets
  exps' <- mapM renderExplosion exps
  text <- renderTextElements g
  let sprites = ship' ++ bullets' ++ enemies' ++ exps' ++ [text] ++ lifeSprites
  return $ Pictures sprites

render (HighScores newHighScore) = do
  scores <- getScores
  header <- renderMessage
              (-300.0) 230.0 $ "Top " ++ (show numHighScores) ++ " scores:"
  let scoreYCoords = take (length scores) [200.0, 170.0..]
  scoreStrings <- sequence $
    zipWith (renderMessage (-285.0)) scoreYCoords (show <$> scores)
  playAgain <- renderMessage 0.0 200.0 $ "'p' to play again"
  menu <- renderMessage 0.0 100.0 $ "'m' for main menu"
  exit <- renderMessage 0.0 0.0 $ "'esc' to exit"
  newHighScoreMessage <- renderMessage 0.0 (-200.0) $
    if (newHighScore)
    then ("New high score!")
    else ("")
  let highScoreElements =
        header:[] ++ scoreStrings ++ playAgain:[] ++
        menu:[] ++ exit:[] ++ newHighScoreMessage:[]
  return $ Pictures highScoreElements

render (GameOver pScore) = do
  score <- renderMessage 0.0 100.0 $ "Final score: " ++ (show pScore)
  playAgain <- renderMessage 0.0 0.0 $ "'p' to play again"
  menu <- renderMessage 0.0 (-100.0) $ "'m' for main menu"
  exit <- renderMessage 0.0 (-200.0) $ "'esc' to exit"
  let gameOverElements = score:[] ++ playAgain:[] ++ menu:[] ++ exit:[]
  return $ Pictures gameOverElements

{- Convenience methd for rendering groups of messages static screens -}
renderTextElements :: GameState -> IO Picture
renderTextElements g = do
  let score =
        renderMessage 0.0 (-280.0) $
          "Current score: " ++ (show $ gPlayerScore g)
  let level = renderMessage 200 (-280.0) $ "Level: " ++ (show $ gLevel g)
  messages <- sequence $ score:[] ++ level:[]
  return $ Pictures messages

{- Convenience method for rendering messages to screen -}
renderMessage :: Float -> Float -> String -> IO Picture
renderMessage xPos yPos message =
  return $ translate xPos yPos $ scale 0.15 0.15 . color white . text $ message

{- The event handlers handles events coming from the user -}
eventHandler :: Event -> GameState -> IO GameState

eventHandler (EventKey key Up _ _) MainMenu
  | key == (Char 'p') = return initState
  | key == (Char 'h') = return $ HighScores False
  | key /= (SpecialKey KeyEsc) = return MainMenu
  | otherwise = case True of
      False -> return MainMenu

eventHandler (EventKey key Up _ _) state@(GameOver _)
  | key == (Char 'p') = return initState
  | key == (Char 'm') = return MainMenu
  | key /= (SpecialKey KeyEsc) = return state
  | otherwise = case True of
      False -> return (GameOver 0)

eventHandler (EventKey key Up _ _) hs@(HighScores _)
  | key == (Char 'p') = return initState
  | key == (Char 'm') = return MainMenu
  | key /= (SpecialKey KeyEsc) = return hs
  | otherwise = case True of
      False -> return hs

eventHandler (EventKey key Up _ _)
  state@(Game ship bullets enemies exps et lives spots score)
  | key == (SpecialKey KeyLeft) = do
      let ship' = moveShip MoveLeft ship
      return $ Game ship' bullets enemies exps et lives spots score
  | key == (SpecialKey KeyRight) = do
      let ship' = moveShip MoveRight ship
      return $ Game ship' bullets enemies exps et lives spots score
  | key == (SpecialKey KeySpace) = do
      return $ Game
                ship
                (bullets ++ [mkShipBullet ship])
                enemies
                exps
                et
                lives
                spots
                score
  | key == (Char 'q') = do
      playerGotHighScore <- evaluateScore score
      if (playerGotHighScore)
      then (return $ HighScores True)
      else (return $ GameOver score)
  | key /= (SpecialKey KeyEsc) = return state
  | otherwise = case True of
      False -> return state

{- The catch all pattern -}
eventHandler _ state = return state
