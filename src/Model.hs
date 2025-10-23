-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Interface.IO.Game

data GameState
  = Running
      { elapsedTime :: Float,
        player :: Player,
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
  | Menu
      { elapsedTime :: Float,
        selectedOption :: Int
      }

initialState :: GameState
initialState = Menu {elapsedTime = 0, selectedOption = 0}

data Player = Player
  { position :: (Float, Float),
    health :: Int,
    ammo :: Int
    -- , playerSprite :: Sprite
  }

initialPlayer :: Player
initialPlayer = Player {position = (-500, 0), health = 1, ammo = 10}

data Bullet = Bullet
  { bulletPos :: (Float, Float),
    bulletSpeed :: Float
    --, bulletSprite :: Sprite
  } deriving Eq

data Enemy = Enemy
  { enemyPos :: (Float, Float),
    enemyDir :: (Float, Float),
    shootInterval :: Float,
    enemyType :: EnemyType
    --, enemySprite :: Sprite
  }

data EnemyType = EnemyStandard | EnemyShooter

enemiesPhase1 :: [Enemy]
enemiesPhase1 =
  [ Enemy {enemyPos = (400, 100), enemyDir = (-1, 2), shootInterval = 2, enemyType = EnemyStandard},
    Enemy {enemyPos = (440, 80), enemyDir = (-1, 2), shootInterval = 2, enemyType = EnemyStandard},
    Enemy {enemyPos = (480, 60), enemyDir = (-1, 2), shootInterval = 2, enemyType = EnemyStandard},
    Enemy {enemyPos = (520, 40), enemyDir = (-1, 2), shootInterval = 2, enemyType = EnemyStandard},
    Enemy {enemyPos = (560, 20), enemyDir = (-1, 2), shootInterval = 2, enemyType = EnemyStandard},
    Enemy {enemyPos = (600, 0), enemyDir = (-1, 2), shootInterval = 2, enemyType = EnemyStandard},   
    Enemy {enemyPos = (600, -150), enemyDir = (-1, 2), shootInterval = 3, enemyType = EnemyShooter}
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