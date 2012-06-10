
module Args 
        ( parseArgs
        , configDefault)
where
import Config
import Model
import Data.Array.Repa                  as R
import Data.Array.Repa.Algorithms.Pixel as R
import Data.Array.Repa.IO.BMP           as R
import qualified Data.Vector.Unboxed    as U
import Prelude                          as P
import System.Console.GetOpt
import System.Exit
import Control.Monad
import Data.IORef
import Data.Char

parseArgs :: [String] -> Config -> IO Config
parseArgs args config
        | []    <- args
        = return config

        | "-batch" : rest       <- args
        = parseArgs rest 
        $ config { configBatchMode      = True }

        | "-frames" : rest      <- args
        = parseArgs rest
        $ config { configFramesMode     = True 
                 , configBatchMode      = True }

        | "-max" : num : rest   <- args
        , all isDigit num
        = parseArgs rest
        $ config { configMaxSteps       = read num }

        | "-width" : num : rest <- args
        , all isDigit num
        = parseArgs rest
        $ config { configModelSize      = (read num, read num) }

        | "-iters" : num : rest <- args
        , all isDigit num
        = parseArgs rest
        $ config { configIters          = read num }

        | "-scale" : int : rest <- args
        = parseArgs rest
        $ config { configScale          = (read int, read int) }

        | "-rate" : int : rest <- args
        = parseArgs rest
        $ config { configRate           = read int }

        | "-delta" : float : rest <- args
        = parseArgs rest
        $ config { configDelta          = read float }

        | "-diff" : float : rest <- args
        = parseArgs rest
        $ config { configDiff           = read float }

        | "-diff-after" : int : rest <- args
        , all isDigit int
        = parseArgs rest
        $ config { configDiffAfter      = read int }

        | "-visc" : float : rest <- args
        = parseArgs rest
        $ config { configVisc           = read float }

        | "-user-dens" : float : rest <- args
        = parseArgs rest
        $ config { configDensity        = read float }

        | "-user-velo" : float : rest <- args
        = parseArgs rest
        $ config { configVisc           = read float }


        -- Initial Confditions ----------------------------------------------------
        | "-dens-bmp" : filePath : rest <- args
        = do    dens    <- loadDensBMP  filePath
                let Z :. height :. width = extent dens
                parseArgs rest
                 $ config { configInitialDensity  = dens
                          , configModelSize       = (width, height) }

        | "-velo-bmp" : filePath : rest <- args
        = do    velo    <- loadVeloBMP filePath
                let Z :. height :. width = extent velo
                parseArgs rest
                 $ config { configInitialVelocity = velo
                          , configModelSize       = (width, height) }

        | "-init-checks" : rest <- args
        = do    let (width, height)     = configModelSize config
                parseArgs rest
                 $ config { configInitialDensity  = makeDensField_checks width height 
                          , configInitialVelocity = makeVeloField_empty  width height }

        | "-init-man" : rest <- args
        = do    let (width, height)     = configModelSize config
                parseArgs rest
                 $ config { configInitialDensity   = makeDensField_checks width height
                          , configInitialVelocity  = makeVeloField_man    width height }

        | "-init-elk" : rest <- args
        = do    let (width, height)     = configModelSize config
                parseArgs rest
                 $ config { configInitialDensity   = makeDensField_checks width height
                          , configInitialVelocity  = makeVeloField_elk    width height }

        | otherwise
        = do    printUsage
                exitWith ExitSuccess


printUsage :: IO ()
printUsage
 = putStr
 $ unlines
        [ "gloss-fluid [flags]"
        , "  -batch                 Run a fixed number of steps instead of displaying in a window."
        , "  -frames                Dump all frames to .bmp files (implies -batch)"
        , "  -max        <INT>      Quit after this number of steps."
        , "  -width      <INT>      Size of simulation.                  (100)"
        , "  -iters      <INT>      Iterations for the linear solver.    (40)"
        , "  -scale      <INT>      Width of a cell in the window.       (5)"
        , "  -rate       <INT>      Frame rate.                          (30)"
        , "  -delta      <FLOAT>    Length of time step.                 (0.1)"
        , "  -diff       <FLOAT>    Diffusion rate for the density.      (0)"
        , "  -diff-after <INT>      Trigger diffusion after this step.   (0)"
        , "  -visc       <FLOAT>    Diffusion rate for the velocity.     (0)"
        , "  -user-dens  <FLOAT>    Magnitude of user inserted density.  (100)"
        , "  -user-velo  <FLOAT>    Magnitude of user inserted velocity. (20)"
        , "  -bmp-dens   <FILE.bmp> File for initial fluid density."
        , "  -bmp-velo   <FILE.bmp> File for initial fluid velocity." 
        , "" ]


configDefault :: Config
configDefault
 = let  modelW  = 100
        modelH  = 100
   in Config
        { configRate            = 30
        , configMaxSteps        = 0
        , configBatchMode       = False
        , configFramesMode      = False
        , configModelSize       = (modelW, modelH)
        , configScale           = (5, 5)
        , configIters           = 40
        , configDelta           = 0.1
        , configDiff            = 0.00001
        , configDiffAfter       = 0
        , configVisc            = 0
        , configDensity         = 100
        , configVelocity        = (20, 20)
        , configInitialDensity  = makeDensField_empty modelW modelH
        , configInitialVelocity = makeVeloField_empty modelW modelH }


-- | Load a density field from a BMP file.
loadDensBMP :: FilePath -> IO DensityField
loadDensBMP filePath
 = do   result   <- readImageFromBMP filePath
        let arr = case result of
                        Right arr'      -> arr'
                        Left  err       -> error $ show err

        let Z :. height' :. width' 
                = extent arr

        density  <- computeUnboxedP 
                 $ R.map floatLuminanceOfRGB8 arr

        return density


-- | Load velocity field from a BMP file.
loadVeloBMP :: FilePath -> IO VelocityField
loadVeloBMP filePath
 = do   result  <- readImageFromBMP filePath
        let arr  = case result of
                        Right arr'      -> arr'
                        Left err        -> error $ show err

        let Z :. height' :. width' 
                = extent arr

        let {-# INLINE conv #-}
            conv (r, g, _b) 
             = let r'   = fromIntegral (-128 + fromIntegral r :: Int)
                   g'   = fromIntegral (-128 + fromIntegral g :: Int)
               in  (r' * 0.0001, g' * 0.0001)

        velocity  <- computeUnboxedP $ R.map conv arr

        return velocity


-------------------------------------------------------------------------------
makeDensField_empty :: Int -> Int -> Array U DIM2 Float
makeDensField_empty width height
        = R.fromUnboxed (Z :. height :. width)
        $ U.replicate (width * height) 0


makeDensField_checks :: Int -> Int -> DensityField
makeDensField_checks width height
 = let  width'  = fromIntegral width
        yc      = fromIntegral (width `div` 2)
        xc      = fromIntegral (width `div` 2)
                        
   in   R.fromListUnboxed (Z :. height :. width)
         $ [ let x'      = fromIntegral (x - 1)
                 y'      = fromIntegral (y - 1)
                 xk1     = cos (10 * (x' - xc) / width')
                 yk1     = cos (10 * (y' - yc) / width')
                 d1      = xk1 * yk1
             in  if (d1 < 0) then 0 else d1
                        | y     <- [1..width]
                        , x     <- [1..width] ]


-------------------------------------------------------------------------------
makeVeloField_empty :: Int -> Int -> Array U DIM2 (Float, Float)
makeVeloField_empty width height
        = R.fromUnboxed (Z :. height :. width)
        $ U.replicate (width * height) (0, 0)


makeVeloField_man :: Int -> Int -> VelocityField
makeVeloField_man width height
 = let  width'  = fromIntegral width
        yc      = fromIntegral (width `div` 2)
        xc      = fromIntegral (width `div` 2)
                        
   in   R.fromListUnboxed (Z :. height :. width)
         $ [ let x'      = fromIntegral x
                 y'      = fromIntegral y
                 xk2     = cos (19 * (x' - xc) / width')
                 yk2     = cos (17 * (y' - yc) / width')
                 d2      = xk2 * yk2 / 5
             in  (0, d2)
                        | y     <- [0..width-1]
                        , x     <- [0..width-1] ]


makeVeloField_elk :: Int -> Int -> VelocityField
makeVeloField_elk width height
 = let  width'  = fromIntegral width
        yc      = fromIntegral (width `div` 2)
        xc      = fromIntegral (width `div` 2)
                        
   in   R.fromListUnboxed (Z :. height :. width)
         $ [ let x'      = fromIntegral x
                 y'      = fromIntegral y
                 xk2     =  cos (12 * (x' - xc) / width')
                 yk2     = -cos (12 * (y' - yc) / width')
                 d2      = xk2 * yk2 / 5
             in  (0, d2)
                        | y     <- [0..width-1]
                        , x     <- [0..width-1] ]

