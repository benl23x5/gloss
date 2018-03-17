
module Solve.Density
        (densitySteps)
where
import Stage.Diffusion
import Stage.Advection
import Stage.Sources
import Config
import Model

-- | Run the stages for processing the density field in one time step
densitySteps
        :: Config
        -> Int
        -> DensityField
        -> Maybe (SourceDensity Float)
        -> VelocityField
        -> IO DensityField

densitySteps config step df ds vf
 = {-# SCC "Solve.densitySteps" #-}
   do   df1     <- addSources   (configDelta config) (configDensity   config)
                                ds df

        let diff = if  configDiffAfter config /= 0
                    && step >= configDiffAfter config
                     then 0.0005
                     else configDiff config

        let diffSolver
                = if configUnstable config
                        then DiffUnstable
                        else DiffStable (configIters config)

        df2     <- diffusion    diffSolver (configDelta config) diff
                                df1

        df'     <- advection    (configDelta config) vf df2

        return  df'
