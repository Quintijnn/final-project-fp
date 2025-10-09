-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import System.Random
import View

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@Menu {selectedOption = opt} =
  case opt of
    1 -> return Running {elapsedTime = 0, player = initialPlayer, enemies = [], bullets = [], rocks = [], score = 0, keysPressed = []}
    2 -> error "Exiting game."
    _ -> return gstate
step secs gstate@Running {elapsedTime = et, keysPressed = ks} =
  let movedState = moveWithKeys ks gstate 
  in return movedState {elapsedTime = et + secs}
step secs gstate@Paused {elapsedTime = et} =
  return gstate {elapsedTime = et + secs}

-- Handle user input
handleInput :: Event -> GameState -> IO GameState
-- Runnning
handleInput (EventKey (SpecialKey key) Down _ _) state@Running {keysPressed = ks}
  | key `elem` [KeyUp, KeyDown] = return state {keysPressed = key : ks}
handleInput (EventKey (SpecialKey key) Up _ _) state@Running {keysPressed = ks}
  | key `elem` [KeyUp, KeyDown] = return state {keysPressed = filter (/= key) ks}

handleInput (EventKey (SpecialKey KeySpace) Down _ _) state@Running {player = pl, bullets = bulls} = return $ fireBullet (position pl) (0, 1) 300 state bulls
handleInput (EventKey (Char 'p') Down _ _) state@Running{} = return $ Paused {elapsedTime = 0, prevState = state}
-- Paused
handleInput (EventKey (Char 'p') Down _ _) state@Paused {} = return $ prevState state
handleInput (EventKey (Char 'q') Down _ _) state@Paused {} = return $ Menu {elapsedTime = 0, selectedOption = 0}
-- Menu
handleInput (EventKey (Char '1') Down _ _) state@Menu {} = return $ state {selectedOption = 1}
handleInput (EventKey (Char '2') Down _ _) state@Menu {} = error "Exiting game."

handleInput _ state = return state

-- Move the player up or down 
movePlayer :: Float -> GameState -> GameState
movePlayer ydelta gstate@Running {player = pl} =
  gstate {player = pl {position = (x, newY)}}
   where
    (x, y) = position pl
    newY = max (-290) (min 290 (y + ydelta))

-- Move the player based on currently pressed keys
moveWithKeys :: [SpecialKey] -> GameState -> GameState
moveWithKeys ks gstate
  | KeyUp `elem` ks    = movePlayer 5 gstate
  | KeyDown `elem` ks  = movePlayer (-5) gstate
  | otherwise           = gstate

fireBullet :: (Float, Float) -> (Float, Float) -> Int -> GameState -> [Bullet] -> GameState
fireBullet (px, py) (dx, dy) speed gstate bulls =
  gstate {bullets = newBullet : bulls}
  where 
    newBullet = Bullet {bulletPos = (px + 5, py), bulletSpeed = speed}

moveBullets :: Float -> [Bullet] -> [Bullet]
moveBullets secs bulls = undefined