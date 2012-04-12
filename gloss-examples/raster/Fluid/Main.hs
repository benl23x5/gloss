{-# LANGUAGE ScopedTypeVariables #-}
module Main
        (main)
where
import Solve.Density
import Solve.Velocity
import Model
import UserEvent
import Args
import Config

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Mem
import System.Environment       (getArgs)
import Data.Array.Repa                  as R
import Data.Array.Repa.IO.BMP           as R


main 
 = do   args    <- getArgs
        config  <- loadConfig args

        case configBatchMode config of
         False -> runInteractive config
         True  -> runBatchMode   config (initModel (configModelSize config))


-- | Run the simulation interactively.
runInteractive config
 =      playIO  (InWindow "fluid" 
                        (configWindowSize config) 
                        (500, 20))
                black
                (configRate config)
                (initModel      (configModelSize config))
                (pictureOfModel (configScale     config))
                (\event model -> return $ userEvent config event model)
                (\_           -> stepFluid config)


-- | Run in batch mode and dump a .bmp of the final state.
runBatchMode :: Config -> Model -> IO ()
runBatchMode config m 
 = do   m'      <- runBatchMode' config m
        outputBMP $ densityField m'

runBatchMode' config model
        | stepsPassed model     > configMaxSteps config
        , configMaxSteps config > 0  
        = return model

        | otherwise     
        = do    model'  <- stepFluid config model
                runBatchMode' config model'



-- Function to step simulator one step forward in time
stepFluid config m@(Model df ds vf vs cl sp cb)
   | sp > configMaxSteps config
   , configMaxSteps config > 0  
   = case configBatchMode config of
                True  -> return m
                False -> error "Finished simulation"

   | otherwise 
   = do performGC 
        vf'     <- velocitySteps config vf vs
        df'     <- densitySteps  config df ds vf'
        return  $ Model df' Nothing vf' Nothing cl (sp + 1) cb


-- Writes bitmap data to test batch-mode ran correctly
outputBMP :: DensityField -> IO ()
outputBMP df 
 = do   arr     <- computeUnboxedP $ R.map pixel8OfDensity df
        R.writeImageToBMP "./output.bmp" arr

