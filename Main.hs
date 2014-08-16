module Main where

import Graphics.Gloss


renderInt :: Int -> Picture
renderInt = scale 0.2 0.2 . text . show

main :: IO ()
main = simulate (InWindow "Nice Window" (200, 200) (800, 200))
                white
                30
                [0..]
                (renderInt . head)
                (\_ _ -> tail)
