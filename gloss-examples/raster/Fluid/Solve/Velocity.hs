
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
import qualified Data.Array.Repa        as R

-- The pass that sets boundary conditions is buggy and 
-- currently disabled.
-- import Stage.Boundary

velocitySteps 
        :: Config
        -> Int
        -> VelocityField 
        -> Maybe (Source (Float, Float)) 
        -> IO VelocityField

velocitySteps config step vf _vs 
 = {-# SCC "Solve.velocitySteps" #-}
   do   -- traceEventIO "Fluid: velocitySteps addSources"
        -- vf1     <- addSources  (configDelta config) (configVelocity config)  vs vf

        -- traceEventIO "Fluid: velocitySteps diffusion"
        -- vf2     <- diffusion   (configDelta config) (configViscosity config) vf1 
        -- vf3     <- setBoundary vf2

        let vf2 = vf

        outputPPM "vel-0start-" step (R.computeS $ R.map ((* 1000) . fst) vf)

        traceEventIO "Fluid: velocitySteps first project"
        vf4     <- project     vf2
        outputPPM "vel-1projA-" step (R.computeS $ R.map ((* 1000) . fst) vf4)
--        vf5     <- setBoundary vf4

        traceEventIO "Fluid: velocitySteps advection"
        vf6     <- advection   (configDelta config) vf4 vf4
        outputPPM "vel-2advec-" step (R.computeS $ R.map ((* 1000) . fst) vf6)
--        vf7     <- setBoundary vf6

        traceEventIO "Fluid: velocitySteps second project"
        vf8     <- project     vf6
        outputPPM "vel-3projB-" step (R.computeS $ R.map ((* 1000) . fst) vf8)
--        vf'     <- setBoundary vf8
        return  vf8
