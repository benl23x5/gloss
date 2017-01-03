
module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Dark side" (400, 400) (10, 10)

background :: Color
background = black

prismSide :: Float
prismSide = 1.0

prismHeight :: Float
prismHeight = prismSide *  sqrt 3.0 / 2.0

prismPath :: [(Float, Float)]
prismPath = [ (0.0, prismHeight)
            , (prismSide * 0.3, 0)
            , (prismSide, 0)
            , (prismSide * 0.4, - prismHeight * 0.4)
            , (prismSide * 0.6, - prismHeight)
            , (0 , - prismHeight * 0.5)
            , (-prismSide * 0.6, - prismHeight)
            , (-prismSide * 0.4, - prismHeight * 0.4)
            , (-prismSide, 0)
            , (-prismSide * 0.3, 0)
            ]

drawing :: Picture
drawing = scale 200 200 $
          pictures [prismBackground
                   , prismBorder
                --    , nekiTekst
                   ]

prismBackground :: Picture
prismBackground = color red $ polygon prismPath
prismBorder :: Picture
prismBorder     = color white $ lineLoop prismPath

nekiTekst :: Picture
nekiTekst = scale 0.01 0.01 $ color blue $ text "e"


main :: IO ()

main = display window background drawing
