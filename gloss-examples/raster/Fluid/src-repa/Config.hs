
module Config
        ( Config (..)
        , configWindowSize)
where
import Model
import Data.Array.Repa                  as R


data Config
        = Config
        { -- | Simulation rate (frames per second)
          configRate            :: Int

          -- | Maximum number of steps in simulation
        , configMaxSteps        :: Int

          -- | Whether to run in batch-mode, non-interactively.
        , configBatchMode       :: Bool

          -- | Whether to dump all frames to .bmp files.
        , configFramesMode      :: Maybe FilePath

          -- | Number of cells in model.
        , configModelSize       :: (Int, Int)

          -- | Window scale.
        , configScale           :: (Int, Int)

          -- | Use the unstable linear solver.
        , configUnstable        :: !Bool

          -- | Number of iterations to use in the linear solver
        , configIters           :: !Int

          -- | Time delta per step.
        , configDelta           :: !Delta

          -- | Diffusion rate.
        , configDiff            :: !Float

          -- | Apply diffusion after this step number.
        , configDiffAfter       :: !Int

          -- | Fluid viscosity.
        , configVisc            :: !Float

          -- | Magnitude of density to add with user interface.
        , configDensity         :: !Float

          -- | Magnitude of velocity to add with user interface.
        , configVelocity        :: !(Float, Float)

          -- | Initial density array
        , configInitialDensity  :: !(Array U DIM2 Float)

          -- | BMP file to use as the initial velocity
        , configInitialVelocity :: !(Array U DIM2 (Float, Float))
        }

configWindowSize :: Config -> (Int, Int)
configWindowSize config
 = let  (modelX, modelY)        = configModelSize config
        (scaleX, scaleY)        = configScale     config
   in   (modelX * scaleX, modelY * scaleY)

