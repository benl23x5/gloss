
module Solve.Velocity
        (velocitySteps)
where
import Stage.Diffusion
import Stage.Advection
import Stage.Sources
import Stage.Project
import Model
import Config
import Debug.Trace

-- The pass that sets boundary conditions is buggy and 
-- currently disabled.
-- import Stage.Boundary

velocitySteps 
        :: Config
        -> VelocityField 
        -> Maybe (Source (Float, Float)) 
        -> IO VelocityField

velocitySteps config vf vs 
 = {-# SCC "Solve.velocitySteps" #-}
   do   traceEventIO "Fluid: velocitySteps addSources"
        vf1     <- addSources  (configDelta config) (configVelocity config)  vs vf

        traceEventIO "Fluid: velocitySteps diffusion"
        vf2     <- diffusion   (configDelta config) (configViscosity config) vf1 
--        vf3     <- setBoundary vf2

        traceEventIO "Fluid: velocitySteps first project"
        vf4     <- project     vf2
--        vf5     <- setBoundary vf4

        traceEventIO "Fluid: velocitySteps advection"
        vf6     <- advection   (configDelta config) vf4 vf4
--        vf7     <- setBoundary vf6

        traceEventIO "Fluid: velocitySteps second project"
        vf8     <- project     vf6
--        vf'     <- setBoundary vf8
        return  vf8
