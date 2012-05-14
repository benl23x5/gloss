
module Solve.Density
        (densitySteps)
where
import Stage.Diffusion
import Stage.Advection
import Stage.Sources
import Config
import Model
import Debug.Trace

-- | Run the stages for processing the density field in one time step
densitySteps 
        :: Config
        -> Int
        -> DensityField 
        -> Maybe (Source Float) 
        -> VelocityField 
        -> IO DensityField

densitySteps config step df ds vf 
 = {-# SCC "Solve.densitySteps" #-}
   do   traceEventIO "Fluid: densitySteps addSources"
        df1     <- addSources (configDelta config) (configDensity   config) ds df

        traceEventIO "Fluid: densitySteps diffusion"
        df2     <- diffusion  (configDelta config) (configDiffusion config) df1

        traceEventIO "Fluid: densitySteps advection"
        df'     <- advection  (configDelta config) vf df2

        traceEventIO "Fluid: densitySteps done"
        return  df'
