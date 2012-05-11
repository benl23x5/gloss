
-- | Stable fluid flow-solver.
--   Based on "Real-time Fluid Dynamics for Games", Jos Stam, Game developer conference, 2003.
--   Implementation by Ben Lambert-Smith.
--   Converted to Repa 3 by Ben Lippmeier.
--
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main)
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
import Data.Array.Repa          as R
import Data.Array.Repa.IO.BMP   as R
import Prelude                  as P
import Debug.Trace


main :: IO ()
main 
 = do   -- Parse the command-line arguments.
        args            <- getArgs
        config          <- loadConfig args

        -- Setup the initial fluid model.
        let model       = initModel 
                                (configInitialDensity  config)
                                (configInitialVelocity config)

        case configBatchMode config of
         False -> runInteractive config model
         True  -> runBatchMode   config model


-- | Run the simulation interactively.
runInteractive :: Config -> Model -> IO ()
runInteractive config model0
 =      playIO  (InWindow "Stam's stable fluid. Use left-click right-drag to add density / velocity." 
                        (configWindowSize config) 
                        (20, 20))
                black
                (configRate config)
                model0
                (pictureOfModel (configScale     config))
                (\event model -> return $ userEvent config event model)
                (\_           -> stepFluid config)


-- | Run in batch mode and dump a .bmp of the final state.
runBatchMode :: Config -> Model -> IO ()
runBatchMode config model
        | stepsPassed model     > configMaxSteps config
        , configMaxSteps config > 0  
        = do    outputBMP $ densityField model
                return ()

        | otherwise     
        = do    model'  <- stepFluid config model
                runBatchMode config model'


-- Function to step simulator one step forward in time
stepFluid :: Config -> Model -> IO Model
stepFluid config m@(Model df ds vf vs cl sp cb)
   | sp                    > configMaxSteps config
   , configMaxSteps config > 0  
   = case configBatchMode config of
                True  -> return m
                False -> error "Finished simulation"

   | otherwise 
   = do performGC 
        traceEventIO $ "stepFluid frame " P.++ show sp P.++ " start"
        putStrLn  $ "Step " ++ show sp
        vf'     <- velocitySteps config vf vs
        df'     <- densitySteps  config df ds vf'
        traceEventIO $ "stepFluid frame " P.++ show sp P.++ " done"
        return  $ Model df' Nothing vf' Nothing cl (sp + 1) cb

-- Writes bitmap data to test batch-mode ran correctly
outputBMP :: DensityField -> IO ()
outputBMP df 
 = do   arr     <- computeUnboxedP $ R.map pixel8OfDensity df
        R.writeImageToBMP "./out.bmp" arr

