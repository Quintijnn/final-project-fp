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
        keysPressed :: [SpecialKey], 
        sprites :: Sprites
      }
  | Paused
      { elapsedTime :: Float,
        prevState :: GameState, 
        sprites :: Sprites
      }
  | GameOver
      { elapsedTime :: Float,
        name :: String,
        score :: Int,
        highScore :: Int, 
        sprites :: Sprites
      }
  | GameVictory
      { elapsedTime :: Float,
        name :: String,
        score :: Int,
        highScores :: [(String, Int)], 
        sprites :: Sprites
      }
  | Menu
      { elapsedTime :: Float,
        selectedOption :: Int,
        sprites :: Sprites
      }

initialState :: GameState
initialState = Menu {elapsedTime = 0, selectedOption = 0}

startMenuState :: Sprites -> GameState
startMenuState s = Menu
  { elapsedTime = 0
  , selectedOption = 0
  , sprites = s
  }

startGameState :: Sprites -> GameState
startGameState s = Running
  { elapsedTime = 0,
    player = initialPlayer (playerS s), 
    enemyPhase = 1,
    enemies = enemiesPhase1 (runnerS s) (shooterS s), 
    bullets = [],
    rocks = [],
    score = 0,
    keysPressed = [], 
    sprites = s
  }

data Player = Player
  { position :: (Float, Float),
    health :: Int,
    ammo :: Int,
    reloadTimer :: Float, 
    playerSprite :: Sprite
  }

initialPlayer :: Sprite -> Player
initialPlayer pSprite = Player {position = (-500, 0), health = 1, ammo = 10, reloadTimer = 0, playerSprite = pSprite}

data Bullet = Bullet
  { bulletPos :: (Float, Float),
    bulletSpeed :: Float, 
    bulletSprite :: Sprite 
  } deriving Eq

data EnemyStatus = Alive | Dying Float
  deriving (Eq)

data Enemy 
  = Shooter
    { enemyPos :: (Float, Float),
      enemyDir :: (Float, Float),
      shootInterval :: Float,
      enemySprite :: Sprite,
      enemyStatus :: EnemyStatus
    } 
  | Runner 
  {
    enemyPos :: (Float, Float),
    enemyDir :: (Float, Float), 
    enemySprite :: Sprite,
    enemyStatus :: EnemyStatus
  }

enemiesPhase1 :: Sprite -> Sprite -> [Enemy]
enemiesPhase1 rS sS =
  [ Runner {enemyPos = (400, 100), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (440, 80), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (480, 60), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (520, 40), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (560, 20), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (600, 0), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},   
    Shooter {enemyPos = (600, -150), enemyDir = (-1, 2), shootInterval = 3, enemySprite = sS, enemyStatus = Alive}
  ]

enemiesPhase2 :: Sprite -> Sprite -> [Enemy]
enemiesPhase2 rS sS =
  [ Runner {enemyPos = (400, 100), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (440, 80), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (480, 60), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Shooter {enemyPos = (520, 40), enemyDir = (-1, 2), shootInterval = 2, enemySprite = sS, enemyStatus = Alive},
    Shooter {enemyPos = (560, 20), enemyDir = (-1, 2), shootInterval = 3, enemySprite = sS, enemyStatus = Alive},
    Shooter {enemyPos = (600, -150), enemyDir = (-1, 2), shootInterval = 3, enemySprite = sS, enemyStatus = Alive}
  ]

enemiesPhase3 :: Sprite -> Sprite -> [Enemy]
enemiesPhase3 rS sS =
  [ Runner {enemyPos = (400, 100), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (440, 80), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (480, 60), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Shooter {enemyPos = (520, 40), enemyDir = (-1, 2), shootInterval = 2, enemySprite = sS, enemyStatus = Alive},
    Shooter {enemyPos = (560, 20), enemyDir = (-1, 2), shootInterval = 3, enemySprite = sS, enemyStatus = Alive},
    Shooter {enemyPos = (600, -150), enemyDir = (-1, 2), shootInterval = 3, enemySprite = sS, enemyStatus = Alive}
  ]

enemiesPhase4 :: Sprite -> Sprite -> [Enemy]
enemiesPhase4 rS sS =
  [ Runner {enemyPos = (400, 100), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (440, 80), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (480, 60), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (500, 40), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (540, 20), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (580, 0), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (640, 20), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (680, 40), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Runner {enemyPos = (720, 60), enemyDir = (-1, 2), enemySprite = rS, enemyStatus = Alive},
    Shooter {enemyPos = (780, 80), enemyDir = (-1, 2), shootInterval = 2, enemySprite = sS, enemyStatus = Alive},
    Shooter {enemyPos = (820, 60), enemyDir = (-1, 2), shootInterval = 2, enemySprite = sS, enemyStatus = Alive},
    Shooter {enemyPos = (860, 40), enemyDir = (-1, 2), shootInterval = 3, enemySprite = sS, enemyStatus = Alive},
    Shooter {enemyPos = (920, 20), enemyDir = (-1, 2), shootInterval = 3, enemySprite = sS, enemyStatus = Alive},
    Shooter {enemyPos = (960, 0), enemyDir = (-1, 2), shootInterval = 3, enemySprite = sS, enemyStatus = Alive},
    Shooter {enemyPos = (1000, -150), enemyDir = (-1, 2), shootInterval = 2, enemySprite = sS, enemyStatus = Alive}
  ]

data Rock = Rock
  { rockSize :: Int,
    rockPos :: (Float, Float),
    flySpeed :: Int,
    rotationSpeed :: Int
  }

data Sprite = Sprite
  { spritePic :: Picture
  , spriteWidth :: Int
  , spriteHeight :: Int
  } deriving Eq

data Sprites = Sprites
  { playerS  :: Sprite
  , runnerS  :: Sprite
  , shooterS :: Sprite
  , bulletS  :: Sprite
  }