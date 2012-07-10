
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

-- | Command line options.
loadConfig :: [String] -> IO Config
loadConfig args
 = do   
        batchModeArg    <- newIORef False
        benchModeArg    <- newIORef False
        framesModeArg   <- newIORef False
        maxStepsArg     <- newIORef 0
        widthArg        <- newIORef 100
        itersArg        <- newIORef 40
        scaleArg        <- newIORef 5
        rateArg         <- newIORef 25
        deltaArg        <- newIORef 0.1
        diffArg         <- newIORef 0.00001
        viscArg         <- newIORef 0
        densArg         <- newIORef 100
        velArg          <- newIORef (20, 20)

        | "-frames" : path : rest <- args
        = parseArgs rest
        $ config { configFramesMode     = Just path
                 , configBatchMode      = True }

        | "-max" : num : rest   <- args
        , all isDigit num
        = parseArgs rest
        $ config { configMaxSteps       = read num }

        | "-size" : width : height : rest <- args
        , all isDigit width
        , all isDigit height
        = parseArgs rest
        $ config { configModelSize      = (read width, read height) }

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

                Option [] ["iters"]             (ReqArg setItersArg     "INT")
                        "Iterations for the linear solver (20)",

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

                Option [] ["diffusion"]         (ReqArg setDiffArg      "FLOAT")
                        "Diffusion rate for the density (0)",

        | "-user-velo" : float : rest <- args
        = parseArgs rest
        $ config { configVisc           = read float }

                Option [] ["viscosity"]         (ReqArg setViscArg      "FLOAT")
                        "Viscosity rate for the velocity (0)",

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
        , "  -frames     <PATH.bmp> Dump all frames to .bmp files (implies -batch)"
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
        , configFramesMode      = Nothing
        , configModelSize       = (modelW, modelH)
        , configScale           = (5, 5)
        , configIters           = 40
        , configDelta           = 0.1
        , configDiff            = 0
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
        height' = fromIntegral height
        xc      = fromIntegral (width  `div` 2)
        yc      = fromIntegral (height `div` 2)
                        
   in   R.fromListUnboxed (Z :. height :. width)
         $ [ let x'      = fromIntegral (x - 1)
                 y'      = fromIntegral (y - 1)
                 tx      = 10 * (x' - xc) / height'
                 ty      = 10 * (y' - yc) / height'
                 xk1     = if abs tx > 3*pi/2 then 0 else cos tx
                 yk1     = if abs ty > 3*pi/2 then 0 else cos ty
                 d1      = xk1 * yk1
             in  if (d1 < 0) then 0 else d1
                        | y     <- [1 .. height]
                        , x     <- [1 .. width] ]


-------------------------------------------------------------------------------
makeVeloField_empty :: Int -> Int -> Array U DIM2 (Float, Float)
makeVeloField_empty width height
        = R.fromUnboxed (Z :. height :. width)
        $ U.replicate (width * height) (0, 0)


makeVeloField_man :: Int -> Int -> VelocityField
makeVeloField_man width height
 = let  width'  = fromIntegral width
        height' = fromIntegral height
        xc      = fromIntegral (width  `div` 2)
        yc      = fromIntegral (height `div` 2)
                        
   in   R.fromListUnboxed (Z :. height :. width)
         $ [ let x'      = fromIntegral x
                 y'      = fromIntegral y
                 xk2     = cos (19 * (x' - xc) / height')
                 yk2     = cos (17 * (y' - yc) / height')
                 d2      = xk2 * yk2 / 5
             in  (0, d2)
                        | y     <- [0..height - 1]
                        , x     <- [0..width  - 1] ]


makeVeloField_elk :: Int -> Int -> VelocityField
makeVeloField_elk width height
 = let  width'  = fromIntegral width
        height' = fromIntegral height
        xc      = fromIntegral (width  `div` 2)
        yc      = fromIntegral (height `div` 2)
                        
   in   R.fromListUnboxed (Z :. height :. width)
         $ [ let x'      = fromIntegral x
                 y'      = fromIntegral y
                 tx      = 12 * (x' - xc) / height'
                 ty      = 12 * (y' - yc) / height'
                 xk2     =  cos tx
                 yk2     = -cos ty
                 d2      = xk2 * yk2 / 5
             in  (0, d2)
                        | y     <- [0 .. height - 1]
                        , x     <- [0 .. width  - 1] ]

