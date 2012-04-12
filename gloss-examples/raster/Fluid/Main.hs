{-# LANGUAGE ScopedTypeVariables #-}
module Main
        (main)
where
import Solve.Density
import Solve.Velocity
import Model
import UserEvent
import Constants
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment( getArgs )
import System.Console.GetOpt
import Data.IORef
import System.Mem
import Prelude as P
import System.IO.Unsafe
import Data.Array.Repa                  as A
import Data.Array.Repa.IO.BMP           as A
import Config

main 
 = do   args <- getArgs
        case getOpt RequireOrder options args of
         (actions,      [],   []) 
          -> foldl (>>) (return ()) actions

         (      _, nonOpts,   []) 
          -> error $ "unrecognized arguments: " P.++ unwords nonOpts

         (      _,       _, msgs) 
          -> error $ concat msgs P.++ usageInfo "Usage: fluid [OPTION...]" options

        case batchMode of
         False -> main'
         True  -> do
                width   <- readIORef widthArg
                runBatchMode (initModel width width)


-- | Command line options.
options :: [OptDescr (IO ())]
options = [
      Option [] ["width"]       (ReqArg getWidthArg       "INT")
         "length of dimensions in fluid simulator",

      Option [] ["dt"]          (ReqArg getDtArg        "FLOAT")
         "size of time step",

      Option [] ["diff"]        (ReqArg getDiffArg      "FLOAT")
         "diffusion rate for the density",

      Option [] ["visc"]        (ReqArg getViscArg      "FLOAT")
         "viscosity rate for the velocity",

      Option [] ["windowWidth"] (ReqArg getWindowWidthArg "INT")
         "length of a side of the window",

      Option [] ["dens"]        (ReqArg getDensArg      "FLOAT")
         "magnitude of a newly inserted density",

      Option [] ["vel"]         (ReqArg getVelArg       "FLOAT")
         "magnitude of a newly inserted velocity",

      Option [] ["rate"]        (ReqArg getRate           "INT")
         "frame rate for simulator",

      Option [] ["maxSteps"]    (ReqArg getMaxSteps       "INT")
         "maximum number of steps for simulator",

      Option [] ["batch-mode"]  (NoArg  batchArg               )
         "sets batch-mode, simulator won't display graphics window"
   ]

-- Writes argument's value into the IORef
getWidthArg       arg = writeIORef widthArg       (read arg)
getDtArg          arg = writeIORef dtArg          (read arg)
getDiffArg        arg = writeIORef diffArg        (read arg)
getViscArg        arg = writeIORef viscArg        (read arg)
getWindowWidthArg arg = writeIORef windowWidthArg (read arg)
getDensArg        arg = writeIORef densArg        (read arg)
getVelArg         arg = let a = read arg in
                           writeIORef velArg (a, a)
getRate           arg = writeIORef rate           (read arg)
getMaxSteps       arg = writeIORef maxStepsArg    (read arg)
batchArg              = writeIORef batchModeArg    True

-- | Specifies the steps per second to take, default is 10
rate :: IORef Int
rate = unsafePerformIO $ newIORef 25


-- | IORef and wrapper function for the batch-mode flag
batchMode :: Bool
batchMode = unsafePerformIO $ readIORef batchModeArg

batchModeArg :: IORef Bool
batchModeArg = unsafePerformIO $ newIORef False


-- | IORef and wrapper function for the maximum steps arg
maxSteps :: Int
maxSteps = unsafePerformIO $ readIORef maxStepsArg

maxStepsArg :: IORef Int
maxStepsArg = unsafePerformIO $ newIORef 0



-- Main -----------------------------------------------------------------------
-- Regular simulator starts here
main' 
 = do   windowWidth      <- readIORef windowWidthArg 
        let windowHeight = windowWidth

        width           <- readIORef widthArg
        let height      = width

        let scaleX      = fromIntegral $ windowWidth `div` width
        let scaleY      = fromIntegral $ windowHeight `div` height
        delta           <- readIORef dtArg
        diff            <- readIORef diffArg
        visc            <- readIORef viscArg
        dens            <- readIORef densArg
        vel             <- readIORef velArg

        let config
                = Config
                { configWindowSize      = (windowWidth, windowHeight)
                , configModelSize       = (width, height)
                , configDelta           = delta
                , configDiffusion       = diff
                , configViscosity       = visc
                , configDensity         = dens
                , configVelocity        = vel }


        playIO  (InWindow "fluid" (windowWidth, windowHeight) (500, 20))
                black
                (unsafePerformIO $ readIORef rate)
                (initModel width height)
                (pictureOfModel scaleX scaleY)
                (\event model -> return $ userEvent config event model)
                stepFluid


-- Function to step simulator one step forward in time
stepFluid _dt m@(Model df ds vf vs cl sp cb)
   | sp > maxSteps
   , maxSteps > 0  
   = case batchMode of
                True  -> return m
                False -> error "Finished simulation"

   | otherwise 
   = do performGC 
        vf'     <- velocitySteps vf vs
        df'     <- densitySteps df ds vf'
        return  $ Model df' Nothing vf' Nothing cl (sp + 1) cb



-- For benchmarking, use this function to run without
-- graphical front-end
runBatchMode :: Model -> IO ()
runBatchMode m 
 = do   m'      <- runBatchMode' m
        outputBMP $ densityField m'

runBatchMode' model
        | stepsPassed model > maxSteps
        , maxSteps > 0  
        = return model

        | otherwise     
        = do    dt      <- readIORef dtArg 
                model'  <- stepFluid dt model
                runBatchMode' model'


-- Writes bitmap data to test batch-mode ran correctly
outputBMP :: DensityField -> IO ()
outputBMP df 
 = do   arr     <- computeUnboxedP $ A.map pixel8OfDensity df
        A.writeImageToBMP "./output.bmp" arr


