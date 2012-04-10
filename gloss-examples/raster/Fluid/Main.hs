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
import Data.Array.Repa          as A
import Data.Array.Repa.Index    as I
import Codec.BMP
import System.Environment( getArgs )
import System.Console.GetOpt
import Data.IORef
import System.Mem
import Prelude as P
import System.IO.Unsafe


main = do
   -- get and process arguments
   args <- getArgs
   case getOpt RequireOrder options args of
      (actions,      [],   []) -> foldl (>>) (return ()) actions
      (      _, nonOpts,   []) -> error $ "unrecognized arguments: " P.++ unwords nonOpts
      (      _,       _, msgs) -> error $ concat msgs P.++ usageInfo "Usage: fluid [OPTION...]" options
   
   main'   

{--
   case batchMode of
      False -> main'
      True  -> runBatchMode model
-}

addArgs []         = return ()
addArgs (arg:args) = arg `seq` (addArgs args)
      
-- Specifies the various command line options
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

-- Regular simulator starts here
main' 
 = playIO
        (InWindow "fluid" (windowWidth, windowHeight) (500, 20))
        black
        (unsafePerformIO $ readIORef rate)
        initModel
        pictureOfModel
        (\event model -> return $ userEvent event model)
        stepFluid


-- Function to step simulator one step forward in time
stepFluid dt m@(Model df ds vf vs cl sp cb)
   | sp > maxSteps
   , maxSteps > 0  
   = case batchMode of
                True  -> return m
                False -> error "Finished simulation"

   | otherwise 
   = do vf'     <- velocitySteps vf vs
        df'     <- densitySteps df ds vf'
        return  $ Model df' Nothing vf' Nothing cl (sp + 1) cb


-- | Specifies the steps per second to take, default is 10
rate :: IORef Int
rate = unsafePerformIO $ newIORef 10


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



{-

-- For benchmarking, use this function to run without
--  graphical front-end
runBatchMode m =
   do outputBMP $ densityField $ runBatchMode' m

runBatchMode' m@(Model df ds vf vs cl sp cb)
   | sp > maxSteps
   , maxSteps > 0  = m
   | otherwise     = performGC `seq` m' `seq`
                     runBatchMode' m'
                   where
                     m' = stepFluid dt m

-- Writes bitmap data to test batch-mode ran correctly
outputBMP :: DensityField -> IO ()
outputBMP df = writeBMP "./output.bmp" $
               packRGBA32ToBMP widthI widthI $
               dfToByteString df

dfToByteString :: DensityField -> B.ByteString
{-# INLINE dfToByteString #-}
dfToByteString df
   = (print bytestring) `seq` bytestring
   where
      bytestring  = RB.toByteString convertedM'
      convertedM' = convertedM A.++ alpha
      convertedM  = A.extend (Z :. A.All :. A.All :. (3::Int)) word8M
      word8M      = A.map (floatToWord8) $ df
      alpha       = A.fromList (Z:.widthI:.widthI:.1) 
                     $ replicate (widthI*widthI) 255
-}
