
module Args where
import Config
import System.Console.GetOpt
import Data.IORef
import Prelude                          as P


-- | Command line options.
loadConfig :: [String] -> IO Config
loadConfig args
 = do   
        batchModeArg    <- newIORef False
        maxStepsArg     <- newIORef 0
        widthArg        <- newIORef 100
        scaleArg        <- newIORef 4
        rateArg         <- newIORef 25
        deltaArg        <- newIORef 0.1
        diffArg         <- newIORef 0
        viscArg         <- newIORef 0
        densArg         <- newIORef 100
        velArg          <- newIORef (20, 20)

        let setWidthArg arg = writeIORef widthArg         (read arg)
        let setScaleArg arg = writeIORef scaleArg         (read arg)
        let setDeltaArg arg = writeIORef deltaArg         (read arg)
        let setDiffArg  arg = writeIORef diffArg          (read arg)
        let setViscArg  arg = writeIORef viscArg          (read arg)
        let setDensArg  arg = writeIORef densArg          (read arg)
        let setVelArg   arg = let a = read arg in writeIORef velArg (a, a)
        let setRate     arg = writeIORef rateArg          (read arg)
        let setMaxSteps arg = writeIORef maxStepsArg      (read arg)
        let setBatchArg     = writeIORef batchModeArg     True

        let options :: [OptDescr (IO ())]
            options
             = [Option [] ["batch"]             (NoArg  setBatchArg            )
                        "Run a fixed number of steps instead of displaying in a window.",

                Option [] ["max"]               (ReqArg setMaxSteps     "INT")
                        "Quit after this number of steps.",

                Option [] ["width"]             (ReqArg setWidthArg     "INT")
                        "Size of simulation (100)",

                Option [] ["scale"]             (ReqArg setScaleArg     "INT")
                        "Width of cell in window (4)",

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
                        "Magnitude of a user inserted velocity (20)"
                ]

        case getOpt RequireOrder options args of
         (actions,      [],   []) 
          -> foldl (>>) (return ()) actions

         (      _, nonOpts,   []) 
          -> error $ "unrecognized arguments: " P.++ unwords nonOpts

         (      _,       _, msgs) 
          -> error $ concat msgs P.++ usageInfo "Usage: fluid [OPTION...]" options


        batchMode       <- readIORef batchModeArg
        maxSteps        <- readIORef maxStepsArg
        width           <- readIORef widthArg
        scale           <- readIORef scaleArg
        rate            <- readIORef rateArg
        delta           <- readIORef deltaArg
        diff            <- readIORef diffArg
        visc            <- readIORef viscArg
        dens            <- readIORef densArg
        vel             <- readIORef velArg

        return  Config
                { configRate            = rate
                , configWindowSize      = (scale * width, scale * width)
                , configMaxSteps        = maxSteps
                , configBatchMode       = batchMode 
                , configModelSize       = (width, width)
                , configDelta           = delta
                , configDiffusion       = diff
                , configViscosity       = visc
                , configDensity         = dens
                , configVelocity        = vel }

