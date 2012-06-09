
module Args where
import Config
import Data.Array.Repa                  as R
import Data.Array.Repa.Algorithms.Pixel as R
import Data.Array.Repa.IO.BMP           as R
import System.Console.GetOpt
import Data.IORef
import Prelude                          as P
import Control.Monad


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

        densityBmpArg   <- newIORef Nothing
        velocityBmpArg  <- newIORef Nothing
        
        let setBatchArg         = writeIORef batchModeArg     True
        let setBenchArg         = writeIORef benchModeArg     True
        let setFramesArg        = writeIORef framesModeArg    True
        let setWidthArg arg     = writeIORef widthArg         (read arg)
        let setScaleArg arg     = writeIORef scaleArg         (read arg)
        let setItersArg arg     = writeIORef itersArg         (read arg)
        let setDeltaArg arg     = writeIORef deltaArg         (read arg)
        let setDiffArg  arg     = writeIORef diffArg          (read arg)
        let setViscArg  arg     = writeIORef viscArg          (read arg)
        let setDensArg  arg     = writeIORef densArg          (read arg)
        let setVelArg   arg     = let a = read arg in writeIORef velArg (a, a)
        let setRate     arg     = writeIORef rateArg          (read arg)
        let setMaxSteps arg     = writeIORef maxStepsArg      (read arg)
        let setDensityBMP  arg  = writeIORef densityBmpArg    (Just arg)
        let setVelocityBMP arg  = writeIORef velocityBmpArg   (Just arg)

        let options :: [OptDescr (IO ())]
            options
             = [Option [] ["batch"]             (NoArg  setBatchArg            )
                        "Run a fixed number of steps instead of displaying in a window.",

                Option [] ["bench"]             (NoArg  setBenchArg            )
                        "Set standard initial conditions for benchmarking.",

                Option [] ["frames"]             (NoArg setFramesArg           )
                        "Dump all frames to .bmp files.",

                Option [] ["max"]               (ReqArg setMaxSteps     "INT")
                        "Quit after this number of steps.",

                Option [] ["width"]             (ReqArg setWidthArg     "INT")
                        "Size of simulation (100)",

                Option [] ["iters"]             (ReqArg setItersArg     "INT")
                        "Iterations for the linear solver (20)",

                Option [] ["scale"]             (ReqArg setScaleArg     "INT")
                        "Width of cell in window (5)",

                Option [] ["rate"]              (ReqArg setRate         "INT")
                        "Frame rate of simulator (25)",

                Option [] ["delta"]             (ReqArg setDeltaArg     "FLOAT")
                        "Length of time steps (0.1)",

                Option [] ["diffusion"]         (ReqArg setDiffArg      "FLOAT")
                        "Diffusion rate for the density (0)",

                Option [] ["viscosity"]         (ReqArg setViscArg      "FLOAT")
                        "Viscosity rate for the velocity (0)",

                Option [] ["user-density"]      (ReqArg setDensArg      "FLOAT")
                        "Magnitude of a user inserted density (100)",

                Option [] ["user-velocity"]     (ReqArg setVelArg       "FLOAT")
                        "Magnitude of a user inserted velocity (20)",

                Option [] ["bmp-density"]       (ReqArg setDensityBMP   "FILE.bmp")
                        "File for initial fluid density",

                Option [] ["bmp-velocity"]      (ReqArg setVelocityBMP  "FILE.bmp")
                        "File for initial fluid velocity"
                ]

        case getOpt RequireOrder options args of
         (actions,      [],   []) 
          -> foldl (>>) (return ()) actions

         (      _, nonOpts,   []) 
          -> error $ "unrecognized arguments: " P.++ unwords nonOpts

         (      _,       _, msgs) 
          -> error $ concat msgs P.++ usageInfo "Usage: fluid [OPTION...]" options


        batchMode       <- readIORef batchModeArg
        benchMode       <- readIORef benchModeArg
        framesMode      <- readIORef framesModeArg
        maxSteps        <- readIORef maxStepsArg
        width           <- readIORef widthArg
        let height      = width
        iters           <- readIORef itersArg
        scale           <- readIORef scaleArg
        rate            <- readIORef rateArg
        delta           <- readIORef deltaArg
        diff            <- readIORef diffArg
        visc            <- readIORef viscArg
        dens            <- readIORef densArg
        vel             <- readIORef velArg
        densityBmp      <- readIORef densityBmpArg
        velocityBmp     <- readIORef velocityBmpArg


        -- Load the initial desity bmp if we were given one.
        let mkInitialDensity
                -- Load density from a .bmp, using the luminance as
                -- the scalar density value.
                | Just filePath <- densityBmp
                = do    result   <- readImageFromBMP filePath
                        let arr  =  case result of
                                        Right arr'      -> arr'
                                        Left err        -> error $ show err

                        let Z :. height' :. width' 
                                 = extent arr

                        when (height /= height' || width /= width')
                         $ error "Fluid: bmp size does not match --width argument"

                        density  <- computeUnboxedP 
                                  $ R.map floatLuminanceOfRGB8 arr
                        return density

                | benchMode
                = let   width'  = fromIntegral width
                        yc      = fromIntegral (width `div` 2)
                        xc      = fromIntegral (width `div` 2)
                        
                  in return
                        $ R.fromListUnboxed (Z :. height :. width)
                        $ [ let x'      = fromIntegral (x - 1)
                                y'      = fromIntegral (y - 1)
                                xk1     = cos (10 * (x' - xc) / width')
                                yk1     = cos (10 * (y' - yc) / width')
                                d1      = xk1 * yk1
                            in  if (d1 < 0) then 0 else d1
                                | y     <- [1..width]
                                , x     <- [1..width] ]

                -- No density file given, so just set the field to zero.
                | otherwise
                = return
                        $ R.fromListUnboxed (Z :. height :. width)
                        $ replicate (height * width) 0


        initialDensity <- mkInitialDensity


        -- Load the initial velocity bmp if we were given one
        let mkInitialVelocity
                -- Load 
                | Just filePath <- velocityBmp
                = do    result  <- readImageFromBMP filePath
                        let arr  =  case result of
                                        Right arr'      -> arr'
                                        Left err        -> error $ show err

                        let Z :. height' :. width' 
                                 = extent arr

                        when (height /= height' || width /= width')
                         $ error "Fluid: bmp size does not match --width argument"

                        let {-# INLINE conv #-}
                            conv (r, g, _b) 
                             = let r'   = fromIntegral (-128 + fromIntegral r :: Int)
                                   g'   = fromIntegral (-128 + fromIntegral g :: Int)
                               in  (r' * 0.0001, g' * 0.0001)

                        velocity  <- computeUnboxedP $ R.map conv arr
                        return velocity

                | benchMode
                = let   width'  = fromIntegral width
                        yc      = fromIntegral (width `div` 2)
                        xc      = fromIntegral (width `div` 2)
                        
                  in return
                        $ R.fromListUnboxed (Z :. height :. width)
                        $ [ let x'      = fromIntegral x
                                y'      = fromIntegral y
                                xk2     = cos (15 * (x' - xc) / width')
                                yk2     = cos (15 * (y' - yc) / width')
                                d2      = xk2 * yk2 / 5
                            in  (0, d2)
                                | y     <- [0..width-1]
                                , x     <- [0..width-1] ]

                -- No velocity file given, so just set the field to zero.
                | otherwise
                = return
                        $ R.fromListUnboxed (Z :. height :. width)
                        $ replicate (height * width) (0, 0)

        initialVelocity <- mkInitialVelocity


        return  Config
                { configRate            = rate
                , configWindowSize      = (scale * width, scale * width)
                , configMaxSteps        = maxSteps
                , configBatchMode       = batchMode 
                , configFramesMode      = framesMode
                , configModelSize       = (width, width)
                , configIters           = iters
                , configDelta           = delta
                , configDiffusion       = diff
                , configViscosity       = visc
                , configDensity         = dens
                , configVelocity        = vel 
                , configInitialDensity  = initialDensity
                , configInitialVelocity = initialVelocity }

