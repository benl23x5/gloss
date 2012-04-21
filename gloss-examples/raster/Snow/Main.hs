{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss.Raster.Array
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa                  as R
import System.Directory
import System.FilePath
import Data.List
import Data.Bits
import Prelude                          as P


main
 = do   config  <- loadConfig
        let (windowX, windowY)     = sizeOfDisplay $ configDisplay config
        let Scale (scaleX, scaleY) = configScale config

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
                (configDisplay config)
                (scaleX, scaleY)
                frame


-- Config ---------------------------------------------------------------------
data Scale 
        = Scale (Int, Int)
        deriving (Read, Show)

data Config
        = Config
        { configDisplay :: Display
        , configScale   :: Scale }

defaultConfig
        = Config
        { configDisplay = InWindow "digital snow" (800, 600) (0, 0)
        , configScale   = Scale (4, 4) }


loadConfig :: IO Config
loadConfig
 = do   dir     <- getCurrentDirectory
        let fileConfig  = dir </> "config.txt"
        exists  <- doesFileExist fileConfig

        case exists of
         False  -> return defaultConfig
         True
          -> do str     <- readFile fileConfig
                let config = foldr parseConfig defaultConfig $ lines str
                return config


parseConfig :: String -> Config -> Config
parseConfig str config
        | isPrefixOf "InWindow" str
        = config { configDisplay = read str }

        | isPrefixOf "FullScreen" str
        = config { configDisplay = read str}

        | isPrefixOf "Scale" str
        = config { configScale   = read str }

        | otherwise
        = config


sizeOfDisplay :: Display -> (Int, Int)
sizeOfDisplay display
 = case display of
        InWindow _ s _  -> s
        FullScreen s    -> s
