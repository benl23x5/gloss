
module Main where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "My Window" (200, 200) (10, 10)) white (Circle 80)

