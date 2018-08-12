
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
import Data.Array.Repa.IO.Timing   as R
import Prelude                     as P


main :: IO ()
main
 = do   -- Parse the command-line arguments.
        args                    <- getArgs
        config                  <- parseArgs args configDefault

        -- Setup the initial fluid model.
        let model       = initModel
                                (configInitialDensity  config)
                                (configInitialVelocity config)

        performGC
        case configBatchMode config of
         False -> runInteractive config model
         True
          -> do (_, elapsed)    <- time $ do   result <- runBatchMode   config model
                                               result `seq` return ()
                putStrLn $ show $ wallTime milliseconds elapsed


-- | Run the simulation interactively.
runInteractive :: Config -> Model -> IO ()
runInteractive config model0
 = let (scaleX, scaleY)  = configScale config
   in  playIO  (InWindow "Stam's stable fluid. Use left-click right-drag to add density / velocity."
                        (configWindowSize config)
                        (20, 20))
                black
                (configRate config)
                model0
                (pictureOfModel (fromIntegral scaleX, fromIntegral scaleY))
                (\event model -> return $ userEvent config event model)
                (\_           -> stepFluid config)


-- | Run in batch mode and dump a .bmp of the final state.
runBatchMode :: Config -> Model -> IO ()
runBatchMode config model
  | stepsPassed model     >= configMaxSteps config
  =     return ()

  | otherwise
  = do  model'  <- stepFluid config model

        case configFramesMode config of
         Nothing -> return ()
         Just path
          -> do putStrLn $ "frame " ++ show (stepsPassed model)
                outputBMP path (stepsPassed model) (densityField model)

        runBatchMode config model'


-- Function to step simulator one step forward in time
stepFluid :: Config -> Model -> IO Model
stepFluid config m@(Model df ds vf vs cl step dv cb)
  | step                  >= configMaxSteps config
  , configMaxSteps config >  0
  = return m

  | otherwise
  = do  performGC
        vf'     <- velocitySteps config step vf vs
        df'     <- densitySteps  config step df ds vf'
        return  $ Model df' Nothing vf' Nothing cl (step + 1) dv cb

