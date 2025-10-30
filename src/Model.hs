-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Interface.IO.Game

data GameState
  = Running
      { elapsedTime :: Float,
        player :: Player,
        enemyPhase :: Int,
        enemies :: [Enemy],
        bullets :: [Bullet],
        rocks :: [Rock],
        score :: Int,
        keysPressed :: [SpecialKey]
      }
  | Paused
      { elapsedTime :: Float,
        prevState :: GameState
      }
  | GameOver
      { elapsedTime :: Float,
        name :: String,
        score :: Int,
        highScores :: [(String, Int)]
      }
  | GameVictory
      { elapsedTime :: Float,
        name :: String,
        score :: Int,
        highScores :: [(String, Int)]
      }
  | Menu
      { elapsedTime :: Float,
        selectedOption :: Int
      }

initialState :: GameState
initialState = Menu {elapsedTime = 0, selectedOption = 0}

startGameState :: GameState
startGameState = Running {elapsedTime = 0, player = initialPlayer, enemyPhase = 1, enemies = enemiesPhase1, bullets = [], rocks = [], score = 0, keysPressed = []}

data Player = Player
  { position :: (Float, Float),
    health :: Int,
    ammo :: Int,
    reloadTimer :: Float
    -- , playerSprite :: Sprite
  }

initialPlayer :: Player
initialPlayer = Player {position = (-500, 0), health = 1, ammo = 10, reloadTimer = 0}

data Bullet = Bullet
  { bulletPos :: (Float, Float),
    bulletSpeed :: Float
    --, bulletSprite :: Sprite
  } deriving Eq

data Enemy 
  = Shooter
    { enemyPos :: (Float, Float),
      enemyDir :: (Float, Float),
      shootInterval :: Float
      --, enemySprite :: Sprite
    } 
  | Runner 
  {
    enemyPos :: (Float, Float),
    enemyDir :: (Float, Float)
    --, enemySprite :: Sprite
  } deriving Eq

enemiesPhase1 :: [Enemy]
enemiesPhase1 =
  [ Runner {enemyPos = (400, 100), enemyDir = (-1, 2)},
    Runner {enemyPos = (440, 80), enemyDir = (-1, 2)},
    Runner {enemyPos = (480, 60), enemyDir = (-1, 2)},
    Runner {enemyPos = (520, 40), enemyDir = (-1, 2)},
    Runner {enemyPos = (560, 20), enemyDir = (-1, 2)},
    Runner {enemyPos = (600, 0), enemyDir = (-1, 2)},   
    Shooter {enemyPos = (600, -150), enemyDir = (-1, 2), shootInterval = 3}
  ]

enemiesPhase2 :: [Enemy]
enemiesPhase2 =
  [ Runner {enemyPos = (400, 100), enemyDir = (-1, 2)},
    Runner {enemyPos = (440, 80), enemyDir = (-1, 2)},
    Runner {enemyPos = (480, 60), enemyDir = (-1, 2)},
    Shooter {enemyPos = (520, 40), enemyDir = (-1, 2), shootInterval = 2},
    Shooter {enemyPos = (560, 20), enemyDir = (-1, 2), shootInterval = 3},
    Shooter {enemyPos = (600, -150), enemyDir = (-1, 2), shootInterval = 3}
  ]

enemiesPhase3 :: [Enemy]
enemiesPhase3 =
  [ Runner {enemyPos = (400, 100), enemyDir = (-1, 2)},
    Runner {enemyPos = (440, 80), enemyDir = (-1, 2)},
    Runner {enemyPos = (480, 60), enemyDir = (-1, 2)},
    Runner {enemyPos = (500, 40), enemyDir = (-1, 2)},
    Runner {enemyPos = (540, 20), enemyDir = (-1, 2)},
    Runner {enemyPos = (580, 0), enemyDir = (-1, 2)},
    Runner {enemyPos = (640, 20), enemyDir = (-1, 2)},
    Runner {enemyPos = (680, 40), enemyDir = (-1, 2)},
    Runner {enemyPos = (720, 60), enemyDir = (-1, 2)},
    Shooter {enemyPos = (780, 80), enemyDir = (-1, 2), shootInterval = 2},
    Shooter {enemyPos = (820, 60), enemyDir = (-1, 2), shootInterval = 2},
    Shooter {enemyPos = (860, 40), enemyDir = (-1, 2), shootInterval = 3},
    Shooter {enemyPos = (920, 20), enemyDir = (-1, 2), shootInterval = 3},
    Shooter {enemyPos = (960, 0), enemyDir = (-1, 2), shootInterval = 3},
    Shooter {enemyPos = (1000, -150), enemyDir = (-1, 2), shootInterval = 2}
  ]

data Rock = Rock
  { rockSize :: Int,
    rockPos :: (Float, Float),
    flySpeed :: Int,
    rotationSpeed :: Int
    --, rockSprite :: Sprite
  }

data Sprite = Sprite
  { image :: String,
    width :: Int,
    height :: Int
  }

initialPlayerSprite :: Sprite
initialPlayerSprite = Sprite {image = "assets/player.png", width = 50, height = 50}