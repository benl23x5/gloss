{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss.Raster.Array
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa                  as R
import Data.Bits
import System.Directory
import Prelude                          as P


main
 = do   (windowX, windowY, scaleX, scaleY)
                <- loadConfig

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
                      in   rgb8 x' x' x'

               in   R.zipWith makePixel arr1 arr2

        animateArray 
                (FullScreen (windowX, windowY))
                (scaleX, scaleY)
                frame


loadConfig :: IO (Int, Int, Int, Int)
loadConfig
 = do   dir     <- getCurrentDirectory
        putStrLn $ "current directory is " P.++ dir
        return  (1440, 900, 4, 4)
