
module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Dark side" (400, 400) (10, 10)

background :: Color
background = black

prismSide = 1.0
prismHeight = prismSide * sqrt(3.0) / 2.0
prismPath = [ (0.0, prismHeight / 2.0)
            , (- prismSide / 2.0, - prismHeight / 2.0)
            , (- prismSide / 2.0,  prismHeight / 2.0)
            , (  0.0, - prismHeight / 2.0)
            ]

drawing :: Picture
drawing = scale 200 200 $
          pictures [prismBackground, prismBorder]

prismBackground = color red $ polygon prismPath
prismBorder     = color white $ lineLoop prismPath



main :: IO ()

main = display window background drawing
