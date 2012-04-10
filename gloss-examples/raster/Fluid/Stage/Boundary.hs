
{-# LANGUAGE BangPatterns #-}
module Stage.Boundary
        (setBoundary)
where
import Model
import FieldElt
import Constants
import Stage.Linear
import Data.Array.Repa          as R
import Data.Array.Repa.Eval     as R
import Data.Vector.Unboxed      (Unbox)
import Control.Monad
import Debug.Trace

setBoundary :: VelocityField -> IO VelocityField
setBoundary f
        = (rebuild f <=< setBoundary <=< grabBorders) f 

-- | Takes the original VelocityField and the array of edges and replaces
--   edge values with new values
rebuild :: VelocityField -> VelocityField -> IO VelocityField
rebuild f e
 = f `deepSeqArray` e `deepSeqArray` 
   do   traceEventIO "Fluid: rebuild"
        computeUnboxedP $ backpermuteDft f rebuildPosMap e
{-# INLINE rebuild #-}


rebuildPosMap :: DIM2 -> Maybe DIM2
rebuildPosMap (Z:.j:.i)
        | j == 0          
        = Just (Z:.0:.i)

        | j == widthI - 1 
        = Just (Z:.1:.i)

        | i == 0          
        = if j == 0        then Just (Z:.0:.0)
          else if j == end then Just (Z:.1:.0)
                           else Just (Z:.2:.j)

        | i == widthI - 1 
        = if j == 0        then Just (Z:.0:.(widthI-1))
          else if j == end then Just (Z:.1:.(widthI-1))
                           else Just (Z:.3:.j)

        | otherwise       = Nothing
        where   end = widthI - 1
{-# INLINE rebuildPosMap #-}


setBoundary' :: VelocityField -> IO VelocityField
setBoundary' e
 = e `deepSeqArray` 
   do   traceEventIO "Fluid: setBoundary'"
        computeUnboxedP $ traverse e id revBoundary
{-# INLINE setBoundary' #-}


-- | Based on position in edges array set the velocity accordingly
revBoundary :: (DIM2 -> (Float,Float)) -> DIM2 -> (Float,Float)
revBoundary loc pos@(Z:.j:.i)
        | j == 0    
        = if i == 0        then grabCornerCase loc (Z:.2:.1) (Z:.0:.1)
          else if i == end then grabCornerCase loc (Z:.0:.(widthI-2)) (Z:.3:.1)
                           else (-p1,p2)
        | j == 1    
        = if i == 0        then grabCornerCase loc (Z:.2:.(widthI-2)) (Z:.1:.1)
          else if i == end then grabCornerCase loc (Z:.1:.(widthI-2)) (Z:.3:.(widthI-2))
                           else (-p1,p2)

        | j == 2        = (p1,-p2)
        | j == 3        = (p1,-p2)
        where (p1,p2)   = loc pos
              end       = widthI - 1
{-# INLINE revBoundary #-}


-- | Corner cases are special and are calculated with this function
grabCornerCase :: (DIM2 -> (Float, Float)) -> DIM2 -> DIM2 -> (Float, Float)
grabCornerCase loc pos1 pos2
 = (p1*q1,p2*q2) ~*~ 0.5
 where  (p1,p2) = loc pos1
        (q1,q2) = loc pos2
{-# INLINE grabCornerCase #-}


-- | Grabs the border elements of the VelocityField and outputs them as
--   one array, for ease of adding back into the original VelocityField later
grabBorders :: VelocityField -> IO VelocityField
grabBorders f
 = f `deepSeqArray` 
   do   traceEventIO "Fluid: grabBorders"
        computeUnboxedP $ backpermute (Z:.4:.widthI) edgeCases f
{-# INLINE grabBorders #-}

-- Maps a position in the edges array to what they were in the original
-- array
edgeCases :: DIM2 -> DIM2
edgeCases (Z:.j:.i)
        | j == 0    = (Z:.0         :.i)
        | j == 1    = (Z:.(widthI-1):.i)
        | j == 2    = (Z:.i         :.0)
        | j == 3    = (Z:.i         :.(widthI-1))
        | otherwise = error "Incorrect coordinate given in setBoundary"
{-# INLINE edgeCases #-}

