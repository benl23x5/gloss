
module Main where

import Graphics.Gloss

main :: IO ()
main    = animate (InWindow "machina" (800, 600) (10, 10))
                  black frame

frame :: Float -> Picture
frame time
        = Scale 0.8 0.8
        $ Rotate (time * 30)
        $ mach time 6

mach :: Float -> Int -> Picture
mach _ 0 = leaf
mach t d
 = Pictures
        [ leaf
        , Translate 0 (-100)
                $ Scale 0.8 0.8
                $ Rotate (90 + t * 30)
                $ mach (t * 1.5) (d - 1)

        , Translate 0   100
                $ Scale 0.8 0.8
                $ Rotate (90 - t * 30)
                $ mach (t * 1.5) (d - 1) ]

leaf :: Picture
leaf    = Pictures
                [ Color (makeColor 1.0 1.0 1.0 0.5) $ Polygon loop
                , Color (makeColor 0.0 0.0 1.0 0.8) $ Line loop ]

loop :: [(Float,Float)]
loop    = [(-10, -100), (-10, 100), (10, 100), (10, -100), (-10, -100)]

