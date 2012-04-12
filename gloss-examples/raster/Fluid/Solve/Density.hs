
module Solve.Density
        (densitySteps)
where
import Stage.Diffusion
import Stage.Advection
import Stage.Sources
import Model
import Constants
import Data.IORef

-- | Run the stages for processing the density field in one time step
densitySteps 
        :: DensityField 
        -> Maybe (Source Float) 
        -> VelocityField 
        -> IO DensityField

densitySteps df ds vf 
 = {-# SCC "Solve.densitySteps" #-}
   do   diff    <- readIORef diffArg
   
        df1     <- addSources ds df
        df2     <- diffusion df1 diff
        df'     <- advection vf df2
        return  df'
