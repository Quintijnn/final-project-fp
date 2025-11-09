module Main where

import Controller
import Model
import View
import Assets
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    playerS  <- playerSpriteAsset
    runnerS  <- runnerSpriteAsset
    shooterS <- shooterSpriteAsset
    bulletS  <- bulletSpriteAsset

    let allSprites = Sprites
                    { playerS = playerS
                    , runnerS = runnerS
                    , shooterS = shooterS
                    , bulletS = bulletS
                    }
    let startMS = startMenuState allSprites

    playIO
      (InWindow "Shoot 'm Up" (1200, 600) (300, 200))
      black
      30
      startMS
      drawState
      handleInput
      step