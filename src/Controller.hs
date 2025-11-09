module Controller where

import Data.List (partition)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import System.Exit (exitSuccess)
import System.Random
import Text.ParserCombinators.ReadP (get)
import View
import Text.Read (readMaybe)
import Control.Exception (try, IOException)
import Data.Maybe (fromMaybe)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@Menu {selectedOption = opt, sprites = s} =
  case opt of
    1 -> return (startGameState s)
    2 -> exitSuccess
    _ -> return gstate
step secs gstate@Running {elapsedTime = et} =
  let movedPlayerState = moveWithKeys secs gstate
      movedBulletsState = moveBullets secs movedPlayerState
      movedEnemiesState = moveEnemies secs movedBulletsState
      enemiesShootState = enemiesShoot secs movedEnemiesState
      checkEnemyCollisionsState = checkEnemyCollisions secs enemiesShootState
      checkPlayerCollisionsState = checkPlayerCollisions secs checkEnemyCollisionsState
      reloadedState = simpleReload secs checkPlayerCollisionsState
      checkPhaseState = checkPhase secs reloadedState
   in return checkPhaseState {elapsedTime = et + secs}
step secs gstate@Paused {elapsedTime = et} =
  return gstate {elapsedTime = et + secs}
step secs gstate@GameOver { elapsedTime = et, score = sc, highScore = hsc } =
  if hsc < 0
    then do
      mh <- readHighScore highScoreFilePath
      let oldHigh = fromMaybe 0 mh
          newHigh = max oldHigh sc
      if sc > oldHigh then writeHighScore highScoreFilePath sc else return ()
      return gstate { elapsedTime = et + secs, highScore = newHigh }
    else
      return gstate { elapsedTime = et + secs }
step secs gstate@GameVictory {elapsedTime = et} =
  return gstate {elapsedTime = et + secs}

-- Handle user input
handleInput :: Event -> GameState -> IO GameState
-- Runnning
handleInput (EventKey (SpecialKey key) Down _ _) state@Running {keysPressed = ks}
  | key `elem` [KeyUp, KeyDown] = return state {keysPressed = key : ks}
handleInput (EventKey (SpecialKey key) Up _ _) state@Running {keysPressed = ks}
  | key `elem` [KeyUp, KeyDown] = return state {keysPressed = filter (/= key) ks}
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state@Running {} = return $ fireBullet (0, 1) 500 state
handleInput (EventKey (Char 'p') Down _ _) state@Running {sprites = s} = return $ Paused {elapsedTime = 0, prevState = state, sprites = s}
-- Paused
handleInput (EventKey (Char 'p') Down _ _) state@Paused {} = return $ prevState state
handleInput (EventKey (Char 'q') Down _ _) state@Paused {sprites = s} = return $ Menu {elapsedTime = 0, selectedOption = 0, sprites = s}
-- Menu
handleInput (EventKey (Char '1') Down _ _) state@Menu {} = return $ state {selectedOption = 1}
handleInput (EventKey (Char '2') Down _ _) state@Menu {} = exitSuccess
-- GameOver
handleInput (EventKey (Char 'r') Down _ _) state@GameOver {sprites = s} = return $ Menu {elapsedTime = 0, selectedOption = 1, sprites = s}
handleInput (EventKey (Char 'q') Down _ _) state@GameOver {} = return $ Menu {elapsedTime = 0, selectedOption = 0}
-- GameVictory
handleInput (EventKey (Char 'r') Down _ _) state@GameVictory {sprites = s} = return $ Menu {elapsedTime = 0, selectedOption = 1, sprites = s}
handleInput (EventKey (Char 'q') Down _ _) state@GameVictory {} = return $ Menu {elapsedTime = 0, selectedOption = 0}
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
moveWithKeys secs gstate@Running {keysPressed = ks}
  | KeyUp `elem` ks && KeyDown `elem` ks = gstate
  | KeyUp `elem` ks = movePlayer secs 300 gstate
  | KeyDown `elem` ks = movePlayer secs (-300) gstate
  | otherwise = gstate

fireBullet :: (Float, Float) -> Float -> GameState -> GameState
fireBullet (dx, dy) speed gstate@Running {player = pl, bullets = bulls, sprites = s} =
  if ammo pl <= 0
    then gstate
    else gstate {player = pl {ammo = ammo pl - 1}, bullets = newBullet : bulls}
  where
    (px, py) = position pl
    newBullet = Bullet
      { bulletPos = (px + dx * 5, py + dy * 5)
      , bulletSpeed = speed
      , bulletSprite = bulletS s
      }



-- Move the bullets and drop off-screen ones
moveBullets :: Float -> GameState -> GameState
moveBullets secs gstate@Running {bullets = bulls} = gstate {bullets = newBullets}
  where
    newBullets = filter isOnScreen $ map moveBullet bulls
    moveBullet (Bullet (bx, by) speed sprite) = Bullet (bx + speed * secs, by) speed sprite
    -- on-screen when x is between -400 and 400 AND y between -300 and 300
    isOnScreen (Bullet (bx, by) _ _) = bx >= (-600) && bx <= 600 && by >= (-300) && by <= 300
moveBullets _ gstate = gstate

moveEnemies :: Float -> GameState -> GameState
moveEnemies secs gstate@Running {enemies = enems} = gstate {enemies = newEnems}
  where
    newEnems = map (`moveEnemy` secs) enems

moveEnemy :: Enemy -> Float -> Enemy
moveEnemy (Runner {enemyPos = (oldX, oldY), enemyDir = (oldDirX, oldDirY), enemySprite = s}) secs = 
  let speed = 75
      (dirX, dirY)
        | oldY > 290 = (oldDirX, -2)
        | oldY < -290 = (oldDirX, 2)
        | otherwise = (oldDirX, oldDirY)
      newX = oldX + dirX * speed * secs
      newY = oldY + dirY * speed * secs
   in Runner {enemyPos = (newX, newY), enemyDir = (dirX, dirY), enemySprite = s} 
moveEnemy (Shooter {enemyPos = (oldX, oldY), enemyDir = (oldDirX, oldDirY), shootInterval = si, enemySprite = s}) secs = 
  let speed = 75
      (dirX, dirY)
        | oldY > 290 = (oldDirX, -2)
        | oldY < -290 = (oldDirX, 2)
        | otherwise = (oldDirX, oldDirY)
      newX = oldX + dirX * speed * secs
      newY = oldY + dirY * speed * secs
   in Shooter {enemyPos = (newX, newY), enemyDir = (dirX, dirY), shootInterval = si, enemySprite = s}

checkEnemyCollisions :: Float -> GameState -> GameState
checkEnemyCollisions secs gstate@Running {bullets = bulls, enemies = enems, score = sc} = gstate {enemies = remainingEnems, bullets = remainingBulls, score = newScore}
  where
    (remainingEnems, remainingBulls, hits) = foldl checkCollision (enems, bulls, 0) (playerBullets bulls)
    newScore = sc + hits * 10

    checkCollision (es, bs, hitCount) b =
      let (hitEnemies, aliveEnemies) = span (isHit b) es
       in let (hitEnemies, aliveEnemies) = partition (isHit b) es
           in if not (null hitEnemies)
                then (aliveEnemies, filter (/= b) bs, hitCount + length hitEnemies)
                else (es, bs, hitCount)

    isHit (Bullet (bx, by) _ _) Shooter {enemyPos = (ex, ey)} =
      let distance = sqrt ((bx - ex) ^ 2 + (by - ey) ^ 2)
       in distance < 20 + 12
    isHit (Bullet (bx, by) _ _) Runner {enemyPos = (ex, ey)} =
      let distance = sqrt ((bx - ex) ^ 2 + (by - ey) ^ 2)
       in distance < 20 + 12

-- enemiesShoot: safe for non-Running states and updates shoot timers
enemiesShoot :: Float -> GameState -> GameState
enemiesShoot secs gstate@Running {enemies = enems, bullets = bulls, sprites = s} = 
  let resetInterval = 3 -- seconds between shots, tweak as needed
      (newEnems, newBulls) = foldr (shootEnemy (bulletS s) secs resetInterval) ([], bulls) enems 
   in gstate {enemies = newEnems, bullets = newBulls}
-- if game state is not Running, leave it unchanged
enemiesShoot _ gs = gs

-- shootEnemy: process one enemy, produce updated enemy list head and possibly a new bullet
shootEnemy :: Sprite -> Float -> Float -> Enemy -> ([Enemy], [Bullet]) -> ([Enemy], [Bullet]) 
shootEnemy bS secs reset e@(Shooter {enemyPos = (ex, ey), shootInterval = si}) (esAcc, bsAcc) 
  | si - secs <= 0 =
      let newBullet = Bullet {bulletPos = (ex - 20, ey), bulletSpeed = -400, bulletSprite = bS} 
          updatedEnemy = e {shootInterval = reset}
       in (updatedEnemy : esAcc, newBullet : bsAcc)
  | otherwise =
      let updatedEnemy = e {shootInterval = si - secs}
       in (updatedEnemy : esAcc, bsAcc)
shootEnemy _ _ _ e (esAcc, bsAcc) = 
  (e : esAcc, bsAcc)

checkPlayerCollisions :: Float -> GameState -> GameState
checkPlayerCollisions secs gstate@Running {player = pl, enemies = enems, score = sc, bullets = bulls, sprites = s} = 
  if any (isCollidingEnem pl) enems || any (isCollidingBull pl) (enemyBullets bulls)
    then GameOver {elapsedTime = 0, name = "", score = sc, highScore = -1, sprites = s} 
    else gstate
  where
    isCollidingEnem Player {position = (px, py)} Shooter {enemyPos = (ex, ey)} =
      let distance = sqrt ((px - ex) ^ 2 + (py - ey) ^ 2)
       in distance < 25 + 20
    isCollidingEnem Player {position = (px, py)} Runner {enemyPos = (ex, ey)} =
      let distance = sqrt ((px - ex) ^ 2 + (py - ey) ^ 2)
       in distance < 25 + 20
    isCollidingBull Player {position = (px, py)} Bullet {bulletPos = (bx, by)} =
      let distance = sqrt ((px - bx) ^ 2 + (py - by) ^ 2)
       in distance < 25 + 20

enemyBullets :: [Bullet] -> [Bullet]
enemyBullets = filter (\b -> bulletSpeed b < 0)

playerBullets :: [Bullet] -> [Bullet]
playerBullets = filter (\b -> bulletSpeed b > 0)

simpleReload :: Float -> GameState -> GameState
simpleReload secs gstate@Running {player = pl@Player {ammo = amm, reloadTimer = rt}}
  | amm >= 10 = gstate {player = pl {reloadTimer = 0}} -- full ammo, reset timer
  | rt + secs >= 1.5 =
      -- reload 1 bullet
      gstate {player = pl {ammo = min 10 (amm + 1), reloadTimer = (rt + secs) - 1.5}}
  | otherwise =
      gstate {player = pl {reloadTimer = rt + secs}}
simpleReload _ gstate = gstate

-- Unused
getRandomPositionX :: IO Float
getRandomPositionX = randomRIO (-400, 600)
-- Unused
getRandomPositionY :: IO Float
getRandomPositionY = randomRIO (-290, 290)
-- Unused
getRandomPosition :: IO (Float, Float)
getRandomPosition = do
  x <- getRandomPositionX
  y <- getRandomPositionY
  return (x, y)

-- Unused
spawnEnemiesAtRandomPositions :: Int -> Enemy -> Sprite -> IO [Enemy]
spawnEnemiesAtRandomPositions n Shooter {} s =
  mapM
    ( \_ -> do
        pos <- getRandomPosition
        return Shooter {enemyPos = pos, enemyDir = (-1, 0), shootInterval = 3, enemySprite = s}
    )
    [1 .. n]
spawnEnemiesAtRandomPositions n Runner {} s =
  mapM
    ( \_ -> do
        pos <- getRandomPosition
        return Runner {enemyPos = pos, enemyDir = (-1, 0), enemySprite = s}
    )
    [1 .. n]

checkPhase :: Float -> GameState -> GameState
checkPhase secs gstate@Running {player = player@Player{ammo = amm}, enemyPhase = enemPhase, enemies = enems, score = sc, sprites = s} 
      | enemPhase == 1 && null enems =
          gstate{player = player{ammo = 10}, enemyPhase = 2, enemies = enemiesPhase2 (runnerS s) (shooterS s), score = sc + 50} 
      | enemPhase == 2 && null enems =
          gstate{player = player{ammo = 10}, enemyPhase = 3, enemies = enemiesPhase3 (runnerS s) (shooterS s), score = sc + 70} 
      | enemPhase == 3 && null enems =
          GameVictory {elapsedTime = 0, name = "", score = sc + 100, highScores = [], sprites = s}
      | otherwise = gstate

checkPhase _ gstate = gstate

-- High score
readHighScore :: FilePath -> IO (Maybe Int)
readHighScore path = do
    contents <- try (readFile path) :: IO (Either IOException String)
    case contents of
      Left _        -> return Nothing
      Right contents -> return (readMaybe contents)

writeHighScore :: FilePath -> Int -> IO ()
writeHighScore path score = writeFile path (show score)

highScoreFilePath :: FilePath
highScoreFilePath = "highscore.txt"

