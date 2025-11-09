-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

drawState :: GameState -> IO Picture
drawState Menu{} = 
  return $ Pictures [ color white $ translate (-100) 0 $ scale 0.3 0.3 $ text "Start - Press 1"
                    , color white $ translate (-100) (-40) $ scale 0.3 0.3 $ text "Exit - Press 2"
                    ]

drawState Running{player = pl, enemies = enems, bullets = bulls, rocks = rks, score = sc} =
  return $ Pictures (playerPicture : bulletPictures ++ enemyPictures ++ [scoreText] ++ [ammoText])
  where 
    playerPicture = drawPlayer pl
    bulletPictures = map drawBullet bulls
    enemyPictures = map drawEnemy enems
    scoreText = drawScore sc
    ammoText = drawAmmo (ammo pl)
drawState Paused{} = 
  return drawPaused
drawState GameOver{score = sc, highScore = hsc} = 
  return $ drawGameOver sc hsc
drawState GameVictory{score = sc} = 
  return $ drawGameVictory sc

drawPlayer :: Player -> Picture
drawPlayer p =
  let (x, y) = position p
  in translate x y (spritePic (playerSprite p))

drawBullet :: Bullet -> Picture
drawBullet b =
  let (x, y) = bulletPos b
  in translate x y (spritePic (bulletSprite b))

drawEnemy :: Enemy -> Picture
drawEnemy e =
  let (x, y) = enemyPos e
      sp     = enemySprite e
      basePic = spritePic sp
      (w, h)  = (fromIntegral (spriteWidth sp), fromIntegral (spriteHeight sp))

      blast t =
        let col | t < 0.10  = white
                | t < 0.20  = yellow
                | t < 0.30  = orange
                | otherwise = red
        in color col (rectangleSolid w h)
  in translate x y $
       case enemyStatus e of
         Alive   -> basePic
         Dying t -> blast t 


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

drawGameOver :: Int -> Int -> Picture
drawGameOver sc hsc = Pictures [ translate (-100) 0 $ scale 0.3 0.3 $ color red $ text "Game Over"
                          , translate (-150) (-40) $ scale 0.15 0.15 $ color white $ text "Press 'r' to restart"
                          , translate (-150) (-80) $ scale 0.15 0.15 $ color white $ text ("Your Score: " ++ show sc)
                          , translate (-150) (-120) $ scale 0.15 0.15 $ color white $ text ("High score: " ++ show hsc)
                          , translate (-150) (-160) $ scale 0.15 0.15 $ color white $ text "Press 'q' to quit to menu"
                          ]
              
drawGameVictory :: Int -> Picture
drawGameVictory sc = Pictures [ translate (-120) 0 $ scale 0.3 0.3 $ color green $ text "You Win!"
                             , translate (-150) (-80) $ scale 0.15 0.15 $ color white $ text ("New highscore: " ++ show sc)
                             , translate (-150) (-40) $ scale 0.15 0.15 $ color white $ text "Press 'r' to restart"
                             , translate (-150) (-120) $ scale 0.15 0.15 $ color white $ text "Press 'q' to quit to menu"
                             ]

drawAmmo :: Int -> Picture
drawAmmo amm = translate (-600) 200 $ scale 0.13 0.13 $ color white $ text ("Ammo: " ++ show amm)