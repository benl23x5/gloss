
module Config 
        ( Config (..)
        , configScale)
where
import Model
import Data.Array.Repa

data Config
        = Config
        { -- | Size of window in pixels
          configWindowSize      :: (Int, Int)

          -- | Simulation rate (frames per second)
        , configRate            :: Int

          -- | Maximum number of steps in simulation
        , configMaxSteps        :: Int

          -- | Whether to run in batch-mode, non-interactively.
        , configBatchMode       :: Bool

          -- | Whether to dump all frames to .bmp files.
        , configFramesMode      :: Bool

          -- | Number of cells in model.
        , configModelSize       :: (Int, Int)

          -- | Number of iterations to use in the linear solver
        , configIters           :: !Int

          -- | Time delta per step.
        , configDelta           :: !Delta

          -- | Diffusion rate.
        , configDiffusion       :: !Float

          -- | Fluid viscosity.
        , configViscosity       :: !Float

          -- | Magnitude of density to add with user interface.
        , configDensity         :: !Float

          -- | Magnitude of velocity to add with user interface.
        , configVelocity        :: !(Float, Float) 

          -- | Initial density array
        , configInitialDensity  :: !(Array U DIM2 Float)

          -- | BMP file to use as the initial velocity
        , configInitialVelocity :: !(Array U DIM2 (Float, Float))
        } 


configScale :: Config -> (Float, Float)
configScale config
 = let  (windowWidth, windowHeight)     = configWindowSize config
        (modelWidth,  modelHeight)      = configModelSize  config
   in   ( fromIntegral windowWidth  / fromIntegral modelWidth
        , fromIntegral windowHeight / fromIntegral modelHeight)
