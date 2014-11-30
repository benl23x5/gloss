{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss.Raster.Array
import System.Environment
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa                  as R
import Data.List
import Data.Bits
import Prelude                          as P

main :: IO ()
main
 = do   args    <- getArgs
        case args of
         []     -> run 800 600 4 4

         [sizeX, sizeY, scaleX, scaleY]
                -> run (read sizeX) (read sizeY) (read scaleX) (read scaleY)

         _ -> putStr $ unlines
           [ "gloss-snow <sizeX::Int> <sizeY::Int> <scaleX::Int> <scaleY::Int>"
           , "    sizeX, sizeY   - visualisation size        (default 800, 600)"
           , "    scaleX, scaleY - pixel scaling factor      (default 4, 4)" ]
   

run :: Int -> Int -> Int -> Int -> IO ()
run windowX windowY scaleX scaleY
 = do
        let !sizeX  = windowX `div` scaleX
        let !sizeY  = windowY `div` scaleY

        let frame time
             = let seed1   = truncate (time * 10000)
                   arr1    = randomishIntArray (Z :. sizeY :. sizeX) 0 255 seed1

                   seed2   = truncate ((time * time) * 100)
                   arr2    = randomishIntArray (Z :. sizeY :. sizeX) 0 255 seed2

                   makePixel i j
                    = let  x       = i + j
                           x'      = x .&. 0x0ff
                      in   rgbI' x' x' x'

               in   R.zipWith makePixel arr1 arr2

        animateArray 
                (InWindow "Digital Snow" (windowX, windowY) (10, 10))
                (scaleX, scaleY)
                frame
