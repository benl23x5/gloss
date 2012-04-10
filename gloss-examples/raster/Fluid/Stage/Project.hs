{-# LANGUAGE BangPatterns #-}
module Stage.Project
        (project)
where
import Model
import FieldElt
import Constants
import Stage.Linear
import Data.Array.Repa          as R
import Data.Array.Repa.Eval     as R
import Data.Vector.Unboxed      (Unbox)


project :: Field (Float, Float) -> IO (Field (Float, Float))
project f
 = f `deepSeqArray` 
   do   let !repeats    = 20
        div     <- computeUnboxedP $ fromFunction (Z:.widthI:.widthI) (genDiv f)
        p       <- linearSolver div div 1 4 repeats
        f'      <- computeUnboxedP $ traverse f id (project' p)
        return f'
{-# INLINE project #-}


-- | Helper for project, used to subtract gradient field from regular field
--   to create mass conserving field
project' 
        :: Field Float
        -> (DIM2 -> (Float, Float)) 
        -> DIM2 -> (Float, Float)
project' p locate pos@(Z:.j:.i)
 = p `deepSeqArray` 
   (locate pos) ~-~ (0.5 * widthF * (p0 - p1),
                     0.5 * widthF * (p2 - p3))
 where
        p0 = useIf (i < widthI-1) (p ! (Z:.j  :.i+1))
        p1 = useIf (i >        0) (p ! (Z:.j  :.i-1))
        p2 = useIf (j < widthI-1) (p ! (Z:.j+1:.i  ))
        p3 = useIf (j >        0) (p ! (Z:.j-1:.i  ))
{-# INLINE project' #-}


-- | Used to create approximate for gradient field
genDiv :: VelocityField -> DIM2 -> Float
genDiv f (Z:.j:.i)
   = (-0.5 * ((u0 - u1) + (v0 - v1))) / widthF
   where
      (u0, _) = useIf (i < widthI-1) (f ! (Z:.j  :.i+1))
      (u1, _) = useIf (i >        0) (f ! (Z:.j  :.i-1))
      ( _,v0) = useIf (j < widthI-1) (f ! (Z:.j+1:.i  ))
      ( _,v1) = useIf (j >        0) (f ! (Z:.j-1:.i  ))
{-# INLINE genDiv #-}
