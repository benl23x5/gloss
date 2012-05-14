

module Main where
import Data.Array.Repa          as R
import System.Environment
import Control.Monad
import Data.Char
import Prelude                  as P

main
 = do   args    <- getArgs
        case args of
         [file1, file2]
          -> do ppm1    <- liftM (R.computeS . R.map fromIntegral) $ loadPPM file1
                ppm2    <- liftM (R.computeS . R.map fromIntegral) $ loadPPM file2
                compareArrays ppm1 ppm2


         _ -> error "bad usage"


loadPPM :: FilePath -> IO (Array U DIM2 Int)
loadPPM path
 = do   str     <- readFile path
        return  $ readPPM str

readPPM :: String -> Array U DIM2 Int
readPPM str
        | "P2" : strXY : strMax : rest         <- lines str
        , [strX, strY]                         <- words strXY
        , all isDigit strX,   x <- (read strX   :: Int)
        , all isDigit strY,   y <- (read strY   :: Int)
        , all isDigit strMax, m <- (read strMax :: Int)
        , ds                    <- words $ concat rest
        , length ds == x * y
        = R.fromListUnboxed (Z :. y :. x) $ P.map read ds

        | otherwise
        = error "ppm read failed"


compareArrays :: Array U DIM2 Float -> Array U DIM2 Float -> IO ()
compareArrays arr1 arr2
 | extent arr1 /= extent arr2
 = error "differing extents"

 | otherwise
 = do   putStrLn $ "extent     = " P.++ (show $ extent arr1)
        putStrLn $ "sum (arr1) = " P.++ (show $ R.sumAllS arr1)
        putStrLn $ "sum (arr2) = " P.++ (show $ R.sumAllS arr2)
        putStrLn $ "rms diff   = " P.++ (show $ sqrt $ R.sumAllS $ R.map (** 2) $ R.zipWith (-) arr1 arr2)
