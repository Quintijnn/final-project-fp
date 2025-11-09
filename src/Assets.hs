module Assets
  ( playerSpriteAsset
  , runnerSpriteAsset
  , shooterSpriteAsset
  , bulletSpriteAsset
  ) where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Model 

-- | Load all sprites
loadSprite :: FilePath -> Int -> Int -> IO Sprite
loadSprite path w h = do
  maybePic <- loadJuicyPNG path
  let pic = maybe (rectangleSolid (fromIntegral w) (fromIntegral h)) id maybePic
  return $ Sprite pic w h

-- These will be initialized in main
playerSpriteAsset :: IO Sprite
playerSpriteAsset  = loadSprite "assets/player.png" 50 50

runnerSpriteAsset :: IO Sprite
runnerSpriteAsset  = loadSprite "assets/runner.png" 40 40

shooterSpriteAsset :: IO Sprite
shooterSpriteAsset = loadSprite "assets/shooter.png" 40 40

bulletSpriteAsset :: IO Sprite
bulletSpriteAsset  = loadSprite "assets/bullet.png" 10 10
