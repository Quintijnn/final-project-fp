-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import System.Random
import View
import System.Exit (exitSuccess)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@Menu {selectedOption = opt} =
  case opt of
    1 -> return startGameState
    2 -> exitSuccess
    _ -> return gstate
step secs gstate@Running {elapsedTime = et} =
  let movedPlayerState = moveWithKeys secs gstate 
      movedBulletsState = moveBullets secs movedPlayerState
      movedEnemiesState = moveEnemies secs movedBulletsState
      checkEnemyCollisionsState = checkEnemyCollisions secs movedEnemiesState
      checkPlayerCollisionsState = checkPlayerCollisions secs checkEnemyCollisionsState
      reloadedState = simpleReload secs checkPlayerCollisionsState
  in return reloadedState {elapsedTime = et + secs}
step secs gstate@Paused {elapsedTime = et} =
  return gstate {elapsedTime = et + secs}
step secs gstate@GameOver {elapsedTime = et} =
  return gstate {elapsedTime = et + secs}

-- Handle user input
handleInput :: Event -> GameState -> IO GameState
-- Runnning
handleInput (EventKey (SpecialKey key) Down _ _) state@Running {keysPressed = ks}
  | key `elem` [KeyUp, KeyDown] = return state {keysPressed = key : ks}
handleInput (EventKey (SpecialKey key) Up _ _) state@Running {keysPressed = ks}
  | key `elem` [KeyUp, KeyDown] = return state {keysPressed = filter (/= key) ks}

handleInput (EventKey (SpecialKey KeySpace) Down _ _) state@Running {player = pl, bullets = bulls} = return $ fireBullet pl (0, 1) 500 state bulls
handleInput (EventKey (Char 'p') Down _ _) state@Running{} = return $ Paused {elapsedTime = 0, prevState = state}
-- Paused
handleInput (EventKey (Char 'p') Down _ _) state@Paused {} = return $ prevState state
handleInput (EventKey (Char 'q') Down _ _) state@Paused {} = return $ Menu {elapsedTime = 0, selectedOption = 0}
-- Menu
handleInput (EventKey (Char '1') Down _ _) state@Menu {} = return $ state {selectedOption = 1}
handleInput (EventKey (Char '2') Down _ _) state@Menu {} = exitSuccess
-- GameOver
handleInput (EventKey (Char 'r') Down _ _) state@GameOver {} = return $ Menu {elapsedTime = 0, selectedOption = 1}
handleInput (EventKey (Char 'q') Down _ _) state@GameOver {} = return $ Menu {elapsedTime = 0, selectedOption = 0}

handleInput _ state = return state

-- Move the player up or down 
movePlayer :: Float -> Float -> GameState -> GameState
movePlayer secs ydelta gstate@Running {player = pl} =
  gstate {player = pl {position = (x, newY)}}
   where
    (x, y) = position pl
    newY = max (-290) (min 290 (y + ydelta * secs))

-- Move the player based on currently pressed keys
moveWithKeys :: Float -> GameState -> GameState
moveWithKeys secs gstate@Running{keysPressed = ks}
  | KeyUp `elem` ks && KeyDown `elem` ks = gstate
  | KeyUp `elem` ks     = movePlayer secs 300 gstate
  | KeyDown `elem` ks   = movePlayer secs (-300) gstate
  | otherwise           = gstate

fireBullet :: Player -> (Float, Float) -> Float -> GameState -> [Bullet] -> GameState
fireBullet player@Player{position = (px, py), ammo = amm} (dx, dy) speed gstate bulls =
  if amm <= 0 then gstate 
  else gstate {player = player {ammo = amm - 1}, bullets = newBullet : bulls}
  where 
    newBullet = Bullet {bulletPos = (px + dx * 5, py + dy * 5), bulletSpeed = speed}

-- Move the bullets and drop off-screen ones
moveBullets :: Float -> GameState -> GameState
moveBullets secs gstate@Running{bullets = bulls} = gstate {bullets = newBullets}
  where
    newBullets = filter isOnScreen $ map moveBullet bulls
    moveBullet (Bullet (bx, by) speed) = Bullet (bx + speed * secs, by) speed
    -- on-screen when x is between -400 and 400 AND y between -300 and 300
    isOnScreen (Bullet (bx, by) _) = bx >= (-500) && bx <= 600 && by >= (-300) && by <= 300 
moveBullets _ gstate = gstate

moveEnemies :: Float -> GameState -> GameState
moveEnemies secs gstate@Running{enemies = enems} = gstate {enemies = newEnems}
  where 
    newEnems = map (`moveEnemy` secs) enems

moveEnemy :: Enemy -> Float -> Enemy
moveEnemy enem@Enemy{enemyPos = (oldX, oldY), enemyDir = (oldDirX, oldDirY)} secs = enem {enemyPos = (newX, newY), enemyDir = (dirX, dirY)}
  where
    (x, y) = enemyPos enem
    speed = 75
    (dirX, dirY)
      | oldY > 290    = (oldDirX, -2)
      | oldY < -290   = (oldDirX, 2)
      | otherwise     = (oldDirX, oldDirY)
    newX = x + dirX * speed * secs
    newY = y + dirY * speed * secs

checkEnemyCollisions :: Float -> GameState -> GameState
checkEnemyCollisions secs gstate@Running{bullets = bulls, enemies = enems, score = sc} = gstate {enemies = remainingEnems, bullets = remainingBulls, score = newScore}
  where 
    (remainingEnems, remainingBulls, hits) = foldl checkCollision (enems, bulls, 0) bulls
    newScore = sc + hits * 10

    checkCollision (es, bs, hitCount) b =
      let (hitEnemies, aliveEnemies) = span (isHit b) es
      in if not (null hitEnemies)
         then (aliveEnemies, filter (/= b) bs, hitCount + length hitEnemies)
         else (es, bs, hitCount)

    isHit (Bullet (bx, by) _) Enemy{enemyPos = (ex, ey)} =
      let distance = sqrt ((bx - ex) ^ 2 + (by - ey) ^ 2)
      in distance < 20 + 12 

checkPlayerCollisions :: Float -> GameState -> GameState
checkPlayerCollisions secs gstate@Running {player = pl, enemies = enems, score = sc} =
  if any (isColliding pl) enems
     then GameOver {elapsedTime = 0, name = "", score = sc, highScores = []}
     else gstate
  where
    isColliding Player{position = (px, py)} Enemy{enemyPos = (ex, ey)} =
      let distance = sqrt ((px - ex) ^ 2 + (py - ey) ^ 2)
      in distance < 25 + 20

simpleReload :: Float -> GameState -> GameState
simpleReload secs gstate@Running { player = pl@Player { ammo = amm, reloadTimer = rt } }
  | amm >= 10 = gstate { player = pl { reloadTimer = 0 } }  -- full ammo, reset timer
  | rt + secs >= 1.5 =
      -- reload 1 bullet
      gstate { player = pl { ammo = min 10 (amm + 1), reloadTimer = (rt + secs) - 1.5 } }
  | otherwise =
      gstate { player = pl { reloadTimer = rt + secs } }
simpleReload _ gstate = gstate

