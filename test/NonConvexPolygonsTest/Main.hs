
module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Dark side" (400, 400) (10, 10)

background :: Color
background = black
prismPath = [(0,0),(4,0),(4,4),(-1,5), (0,4), (3,3), (2.9,1), (1,1), (1,2.9)]--,-- (0,4)]
drawing = scale 200 200 $ pictures
          [prismBackground
          ]

prismBackground = color red $ polygon prismPath

nekiTekst =  scale 0.01 0.01 $ color blue $ text "ћћћ ччч шшшThe quick brown fox"
nekiTekst2 = scale 0.01 0.01 $ translate 50 50 $ color red $ text  " jump over the lazy dog"

main :: IO ()

main = display window background drawing
