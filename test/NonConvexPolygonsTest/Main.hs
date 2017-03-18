module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Test" (400, 400) (10, 10)

background :: Color
background = black

testSide :: Float
testSide = 1.0

testHeight :: Float
testHeight = testSide *  sqrt 3.0 / 2.0

-- testPath :: [(Float, Float)]
-- testPath = [ (0.0, testHeight)
--             , (testSide * 0.3, 0)
--             , (testSide, 0)
--             , (testSide * 0.4, - testHeight * 0.4)
--             , (testSide * 0.6, - testHeight)
--             , (0 , - testHeight * 0.5)
--             , (-testSide * 0.6, - testHeight)
--             , (-testSide * 0.4, - testHeight * 0.4)
--             , (-testSide, 0)
--             , (-testSide * 0.3, 0)
--             ]
--
testPath :: [(Float, Float)]
testPath = [ (testSide * (-0.6), testHeight * 0.7) -- 1
            , (testSide * 0.7, testHeight * 0.7) -- 2
            , (testSide * 0.7, testHeight * (-0.7)) -- 3
            , (testSide * (-0.7), testHeight * (-0.7)) -- 4
            , (testSide * (-0.7), testHeight * 0.6) -- 5
            , (testSide * (-0.5), testHeight * 0.4) -- 6
            , (testSide * (-0.55), testHeight * (-0.55)) -- 7
            , (testSide * 0.5, testHeight * (-0.5)) -- 8
            , (testSide * 0.5, testHeight * 0.5) -- 9
            , (testSide * (-0.4), testHeight * 0.5) -- 10
            ]


drawing :: Picture
drawing = scale 200 200 $
          pictures [testBackground
                   , testBorder
                --    , nekiTekst
                   ]

testBackground :: Picture
testBackground = color red $ polygon testPath
testBorder :: Picture
testBorder     = color white $ lineLoop testPath

nekiTekst :: Picture
nekiTekst = scale 0.01 0.01 $ color blue $ text "e"


main :: IO ()

main = display window background drawing
