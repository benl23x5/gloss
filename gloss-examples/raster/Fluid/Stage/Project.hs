{-# LANGUAGE BangPatterns #-}
module Stage.Project
        (project)
where
import Model
import FieldElt
import Constants
import Stage.Linear
import Data.Array.Repa          as R
import Data.Array.Repa.Unsafe   as R
import Debug.Trace
import Data.IORef

project :: Field (Float, Float) -> IO (Field (Float, Float))
project f
 = {-# SCC project #-}
   f `deepSeqArray` 
   do   let !repeats    = 20
        !width  <- readIORef widthArg

        traceEventIO "Fluid: project gradient"
        divergence <- {-# SCC "project.genDiv" #-}
                   computeUnboxedP $ fromFunction (Z:. width :. width) (genDiv width f)

        traceEventIO "Fluid: project linear solver"
        p       <- {-# SCC "project.linearSolver" #-}
                   linearSolver divergence divergence 1 4 repeats

        traceEventIO "Fluid: project apply"
        f'      <- {-# SCC "project.apply" #-}
                   computeUnboxedP $ unsafeTraverse f id (project' width p)
        return f'
{-# NOINLINE project #-}


-- | Helper for project, used to subtract gradient field from regular field
--   to create mass conserving field
project' 
        :: Int                          -- ^ Width of model.
        -> Field Float
        -> (DIM2 -> (Float, Float)) 
        -> DIM2 
        -> (Float, Float)

project' !width !p !locate !pos@(Z:.j:.i)
 = (locate pos) ~-~ (0.5 * width' * (p0 - p1),
                     0.5 * width' * (p2 - p3))
 where
        !width' = fromIntegral width
        !p0     = useIf (i < width - 1) (p `unsafeIndex` (Z :. j   :. i+1))
        !p1     = useIf (i >         0) (p `unsafeIndex` (Z :. j   :. i-1))
        !p2     = useIf (j < width - 1) (p `unsafeIndex` (Z :. j+1 :. i  ))
        !p3     = useIf (j >         0) (p `unsafeIndex` (Z :. j-1 :. i  ))
{-# INLINE project' #-}


-- | Used to create approximate for gradient field
genDiv :: Int -> VelocityField -> DIM2 -> Float
genDiv !width !f (Z :. j :. i)
 = (-0.5 * ((u0 - u1) + (v0 - v1))) / (fromIntegral width)
 where
      (u0, _) = useIf (i < width - 1) (f `unsafeIndex` (Z:.j  :.i+1))
      (u1, _) = useIf (i >         0) (f `unsafeIndex` (Z:.j  :.i-1))
      ( _,v0) = useIf (j < width - 1) (f `unsafeIndex` (Z:.j+1:.i  ))
      ( _,v1) = useIf (j >         0) (f `unsafeIndex` (Z:.j-1:.i  ))
{-# INLINE genDiv #-}
