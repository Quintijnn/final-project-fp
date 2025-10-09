-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- view :: GameState -> IO Picture
-- view = return . drawState

-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])

drawState :: GameState -> IO Picture
drawState Menu{} = 
  return $ Pictures [ color white $ translate (-100) 0 $ scale 0.3 0.3 $ text "Start - Press 1"
                    , color white $ translate (-100) (-40) $ scale 0.3 0.3 $ text "Exit - Press 2"
                    ]

drawState Running{player = pl, enemies = enems, bullets = bulls, rocks = rks, score = sc} =
  return $ Pictures (playerPicture : bulletPictures ++ enemyPictures ++ [scoreText])
  where 
    playerPicture = drawPlayer pl
    bulletPictures = map drawBullet bulls
    enemyPictures = map drawEnemy enems
    scoreText = drawScore sc
drawState Paused{} = 
  return drawPaused
drawState GameOver{score = sc} = 
  return $ drawGameOver sc

drawPlayer :: Player -> Picture
drawPlayer Player{position = (x, y)} =
  translate x y (color white (circleSolid 10))

drawBullet :: Bullet -> Picture
drawBullet Bullet{bulletPos = (x, y)} =
  translate x y (color red (circleSolid 5)) 

drawEnemy :: Enemy -> Picture
drawEnemy Enemy{enemyPos = (x, y), enemyType = et} =
  translate x y (color blue (case et of
                                EnemyStandard -> circleSolid 8
                                EnemyShooter  -> rectangleSolid 12 12))

drawRock :: Rock -> Picture
drawRock Rock{rockPos = (x, y), rockSize = size} =
  translate x y (color white (circleSolid (fromIntegral size))) 

drawScore :: Int -> Picture
drawScore sc = translate (-600) 250 $ scale 0.15 0.15 $ color white $ text ("Score: " ++ show sc) 

drawPaused :: Picture
drawPaused = Pictures [ translate (-100) 0 $ scale 0.3 0.3 $ color yellow $ text "Game Paused"
                      , translate (-150) (-80) $ scale 0.15 0.15 $ color white $ text "Press 'p' to return"
                      , translate (-150) (-120) $ scale 0.15 0.15 $ color white $ text "Press 'q' to quit to menu"
                      ]

drawGameOver :: Int -> Picture
drawGameOver sc = Pictures [ translate (-100) 0 $ scale 0.3 0.3 $ color red $ text "Game Over"
                          , translate (-150) (-80) $ scale 0.15 0.15 $ color white $ text ("Your Score: " ++ show sc)
                          , translate (-150) (-40) $ scale 0.15 0.15 $ color white $ text "Press 'r' to restart"
                          , translate (-150) (-120) $ scale 0.15 0.15 $ color white $ text "Press 'q' to quit to menu"
                          ]