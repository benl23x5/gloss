{-# LANGUAGE BangPatterns #-}
module Stage.Advection
        (advection)
where
import Model
import FieldElt
import Constants
import Stage.Linear
import Data.Array.Repa          as R
import Data.Array.Repa.Unsafe   as R
import Data.Array.Repa.Eval     as R
import Data.Vector.Unboxed      (Unbox)
import Debug.Trace
import System.IO.Unsafe
import Data.IORef

advection 
        :: (FieldElt a, Unbox a)
        => VelocityField -> Field a -> IO (Field a)

advection vf f
 = {-# SCC "advection" #-} 
   vf `deepSeqArray` f `deepSeqArray`
   do   traceEventIO "Fluid: advection"
        computeP $ unsafeTraverse f id (advection' vf f)


{-# SPECIALIZE advection 
        :: VelocityField -> Field Float
        -> IO (Field Float) #-}

{-# SPECIALIZE advection 
        :: VelocityField -> Field (Float, Float)
        -> IO (Field (Float, Float)) #-}


-- Helper to calculate density to be placed at each point
advection' 
        :: (FieldElt a, Unbox a)
        => VelocityField 
        -> Field a
        -> (DIM2 -> a) 
        -> DIM2 
        -> a

advection' vf orig locate pos@(Z:.j:.i)
 = vf `deepSeqArray` orig `deepSeqArray`
      (((d00 ~*~ t0) ~+~ (d01 ~*~ t1)) ~*~ s0) 
  ~+~ (((d10 ~*~ t0) ~+~ (d11 ~*~ t1)) ~*~ s1)
 where
        !width  = fromIntegral $ unsafePerformIO $ readIORef widthArg

        -- backtrack densities to point based on velocity field
        -- and make sure they are in field
        !x = checkLocation width $ fromIntegral i - dt0 * u
        !y = checkLocation width $ fromIntegral j - dt0 * v

        -- calculate discrete locations surrounding point
        !i0 = truncate x
        !i1 = i0 + 1

        !j0 = truncate y
        !j1 = j0 + 1

        -- calculate ratio point is between the discrete locations
        !s1 = x - fromIntegral i0
        !s0 = 1 - s1

        !t1 = y - fromIntegral j0
        !t0 = 1 - t1

        -- grab values from grid surrounding advected point
        !d00 = orig `unsafeIndex` (Z:. j0 :. i0)
        !d01 = orig `unsafeIndex` (Z:. j1 :. i0)
        !d10 = orig `unsafeIndex` (Z:. j0 :. i1)
        !d11 = orig `unsafeIndex` (Z:. j1 :. i1)

        -- helper values

        !dt0    = dt * width
        !(u, v) = vf `unsafeIndex` pos

{-# SPECIALIZE advection' 
        :: VelocityField -> Field Float -> (DIM2 -> Float) 
        -> DIM2 -> Float #-}

{-# SPECIALIZE advection' 
        :: VelocityField -> Field (Float, Float) 
        -> (DIM2 -> (Float,Float))
        -> DIM2  -> (Float,Float) #-}


-- Safety check that we are within the simulation area
checkLocation :: Float -> Float -> Float
checkLocation width x
   | x < 0.5          = 0.5
   | x > width - 1.5 = width - 1.5
   | otherwise        = x
{-# INLINE checkLocation #-}
