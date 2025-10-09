module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Shoot 'm Up" (1200, 600) (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              initialState     -- Initial state
              drawState             -- View function
              handleInput            -- Event function
              step             -- Step function
