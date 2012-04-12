
module Config 
        ( Config (..)
        , configScale)
where
import Model

data Config
        = Config
        { -- | Size of window in pixels
          configWindowSize      :: (Int, Int)

          -- | Number of cells in model.
        , configModelSize       :: (Int, Int)

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
        } 


configScale :: Config -> (Float, Float)
configScale config
 = let  (windowWidth, windowHeight)     = configWindowSize config
        (modelWidth,  modelHeight)      = configModelSize  config
   in   ( fromIntegral windowWidth  / fromIntegral modelWidth
        , fromIntegral windowHeight / fromIntegral modelHeight)
