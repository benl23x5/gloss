
module Main where
import Graphics.Gloss

-- Check that arcs with width 0 are drawn the same way around as arcs with
-- width greater than 0.
main
 = animate (InWindow "Nice Window" (200, 300) (10, 10)) white
 $ \time
 -> pictures
        [ translate 0   0   $ rotate (time * 50) $ thickArc 180   0 80 0
        , translate 0 (-50) $ rotate (time * 50) $ thickArc 180   0 80 5
        , translate 0   50  $ rotate (time * 50) $ thickArc 180 360 80 5 ]

