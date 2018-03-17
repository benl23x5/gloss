
module Solve.Velocity
        (velocitySteps)
where
import Stage.Diffusion
import Stage.Advection
import Stage.Sources
import Stage.Project
import Model
import Config

-- The pass that sets boundary conditions is buggy and
-- currently disabled.
-- import Stage.Boundary
velocitySteps
        :: Config
        -> Int
        -> VelocityField
        -> Maybe (SourceDensity (Float, Float))
        -> IO VelocityField

velocitySteps config _step vf vs
 = {-# SCC "Solve.velocitySteps" #-}
   do
        vf1     <- addSources   (configDelta config) (configVelocity config)
                                vs vf

        let diffSolver = DiffStable (configIters config)
        vf2     <- diffusion    diffSolver (configDelta config) (configVisc config)
                                vf1
--      vf3     <- setBoundary vf2

        vf4     <- project      (configIters config) vf2
--      vf5     <- setBoundary vf4

        vf6     <- advection    (configDelta config) vf4 vf4
--      vf7     <- setBoundary vf6

        vf8     <- project      (configIters config) vf6
--      vf'     <- setBoundary vf8

        return  vf8
