-- | This module contains the data types
--   which represent the state of the game
module Model where

data GameState
  = Running
      { player :: Player,
        enemies :: [Enemy],
        bullets :: [Bullet],
        rocks :: [Rock],
        score :: Int
      }
  | Paused
      { prevState :: GameState
      }
  | GameOver
      { name :: String,
        score :: Int,
        highScores :: [(String, Int)]
      }
  | Menu
      { selectedOption :: Int
      }

data Player = Player
  { position :: (Float, Float),
    health :: Int,
    playerSprite :: Sprite
  }

data Bullet = Bullet
  { bulletPos :: (Float, Float),
    bulletSpeed :: Int,
    bulletSprite :: Sprite
  }

data Enemy = Enemy
  { enemyPos :: (Float, Float),
    enemyDir :: (Float, Float),
    shootInterval :: Float,
    enemyType :: EnemyType,
    enemySprite :: Sprite
  }

data EnemyType = EnemyStandard | EnemyShooter

data Rock = Rock
  { rockSize :: Int,
    rockPos :: (Float, Float),
    flySpeed :: Int,
    rotationSpeed :: Int,
    rockSprite :: Sprite
  }

data Sprite = Sprite
  { image :: String,
    width :: Int,
    height :: Int
  }