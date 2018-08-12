{-# LANGUAGE BangPatterns #-}

module Stage.Boundary
        (setBoundary)
where
import Model
import Data.Array.Repa          as R
import Control.Monad
import Debug.Trace
import Config
import FieldElt

-- | Apply boundary conditions to a velocity field.
setBoundary :: Config -> VelocityField -> IO VelocityField
setBoundary config f
 = let  (width, _)      = configModelSize config
   in   (rebuild width f <=< setBoundary' width <=< grabBorders width) f


-- | Takes the original VelocityField and the array of edges and replaces
--   edge values with new values
rebuild :: Int -> VelocityField -> VelocityField -> IO VelocityField
rebuild width field edges
 = field `deepSeqArray` edges `deepSeqArray`
   do   computeUnboxedP $ backpermuteDft field (rebuildPosMap width) edges
{-# INLINE rebuild #-}


rebuildPosMap :: Int -> DIM2 -> Maybe DIM2
rebuildPosMap !width (Z:.j:.i)
        | j == 0
        = Just (Z:.0:.i)

        | j == width - 1
        = Just (Z:.1:.i)

        | i == 0
        = if j == 0        then Just (Z:. 0 :. 0)
          else if j == end then Just (Z:. 1 :. 0)
                           else Just (Z:. 2 :. j)

        | i == width - 1
        = if j == 0        then Just (Z:. 0 :. (width-1))
          else if j == end then Just (Z:. 1 :. (width-1))
                           else Just (Z:. 3 :. j)

        | otherwise       = Nothing
        where   end = width - 1
{-# INLINE rebuildPosMap #-}



-- | Grabs the border elements of the VelocityField and outputs them as
--   one array, for ease of adding back into the original VelocityField later
grabBorders :: Int -> VelocityField -> IO VelocityField
grabBorders width f
 = f `deepSeqArray`
   do   traceEventIO "Fluid: grabBorders"
        computeUnboxedP $ backpermute (Z:. 4 :. width) (edgeCases width) f
{-# INLINE grabBorders #-}


-- | Map a position in the edges array to what they were in the original
--   array.
edgeCases :: Int -> DIM2 -> DIM2
edgeCases width (Z:.j:.i)
        | j == 0    = (Z:.0         :.i)
        | j == 1    = (Z:.(width-1) :.i)
        | j == 2    = (Z:.i         :.0)
        | j == 3    = (Z:.i         :.(width-1))
        | otherwise = error "Incorrect coordinate given in setBoundary"
{-# INLINE edgeCases #-}



setBoundary' :: Int -> VelocityField -> IO VelocityField
setBoundary' width e
 = e `deepSeqArray`
   do   traceEventIO "Fluid: setBoundary'"
        computeUnboxedP $ traverse e id (revBoundary width)
{-# INLINE setBoundary' #-}


-- | Based on position in edges array set the velocity accordingly
revBoundary :: Int -> (DIM2 -> (Float,Float)) -> DIM2 -> (Float,Float)
revBoundary width loc pos@(Z:.j:.i)
        | j == 0
        = if i == 0        then grabCornerCase loc (Z:.2:.1) (Z:.0:.1)
          else if i == end then grabCornerCase loc (Z:.0:.(width-2)) (Z:.3:.1)
                           else (-p1,p2)
        | j == 1
        = if i == 0        then grabCornerCase loc (Z:.2:.(width-2)) (Z:.1:.1)
          else if i == end then grabCornerCase loc (Z:.1:.(width-2)) (Z:.3:.(width-2))
                           else (-p1,p2)

        | j == 2        = (p1,-p2)
        | j == 3        = (p1,-p2)

        | otherwise     = error "Fluid: revBoundary"
        where (p1,p2)   = loc pos
              end       = width - 1


{-# INLINE revBoundary #-}

-- | Corner cases are special and are calculated with this function
grabCornerCase :: (DIM2 -> (Float, Float)) -> DIM2 -> DIM2 -> (Float, Float)
grabCornerCase loc pos1 pos2
 = (p1 * q1, p2 * q2) ~*~ 0.5
 where  (p1,p2) = loc pos1
        (q1,q2) = loc pos2
{-# INLINE grabCornerCase #-}



