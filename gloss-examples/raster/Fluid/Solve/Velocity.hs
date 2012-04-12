
module Solve.Velocity
        (velocitySteps)
where
import Stage.Diffusion
import Stage.Advection
import Stage.Sources
import Stage.Project
import Model
import Constants
import Data.IORef

velocitySteps 
        :: VelocityField 
        -> Maybe (Source (Float, Float)) 
        -> IO VelocityField

velocitySteps vf vs 
 = {-# SCC "Solve.velocitySteps" #-}
   do   visc            <- readIORef $ viscArg
        delta           <- readIORef $ dtArg
        velocity        <- readIORef $ velArg

        vf1     <- addSources  delta velocity vs vf
        vf2     <- diffusion   delta visc vf1 
--        vf3     <- setBoundary vf2
        vf4     <- project     vf2
--        vf5     <- setBoundary vf4
        vf6     <- advection   delta vf4 vf4
--        vf7     <- setBoundary vf6
        vf8     <- project     vf6
--        vf'     <- setBoundary vf8
        return  vf8
