{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stage.Advection
        (advection)
where
import Model
import FieldElt
import Data.Array.Repa          as R
import Data.Array.Repa.Unsafe   as R
import Data.Vector.Unboxed      (Unbox)


-- | Apply a velocity field to another field.
--   Both fields must have the same extent.
advection
        :: (FieldElt a, Unbox a)
        => Delta
        -> VelocityField
        -> Field a
        -> IO (Field a)

advection !delta velField field
 = {-# SCC "advection" #-}
   velField `deepSeqArray` field `deepSeqArray`
   do   computeP $ unsafeTraverse field id (advectElem delta velField)

{-# SPECIALIZE advection
        :: Delta
        -> VelocityField -> Field Float
        -> IO (Field Float) #-}

{-# SPECIALIZE advection
        :: Delta
        -> VelocityField -> Field (Float, Float)
        -> IO (Field (Float, Float)) #-}


-- | Compute the new field value at the given location.
advectElem
        :: (FieldElt a)
        => Delta                -- ^ Time delta (in seconds)
        -> VelocityField        -- ^ Velocity field that moves the source field.
        -> (DIM2 -> a)          -- ^ Get an element from the source field.
        -> DIM2                 -- ^ Compute the new value at this index.
        -> a

advectElem !delta !velField !get !pos@(Z:. j :. i)
 = velField `deepSeqArray`
      (((d00 ~*~ t0) ~+~ (d01 ~*~ t1)) ~*~ s0)
  ~+~ (((d10 ~*~ t0) ~+~ (d11 ~*~ t1)) ~*~ s1)
 where
        _ :. height' :. width' = R.extent velField
        !width          = fromIntegral width'
        !height         = fromIntegral height'

        -- helper values
        !dt0    = delta * width
        !(u, v) = velField `unsafeIndex` pos

        -- backtrack densities to point based on velocity field
        -- and make sure they are in field
        !x      = fromIntegral i - dt0 * u
        !y      = fromIntegral j - dt0 * v

        !x'     | x < -0.5              = -0.5
                | x > width + 0.5       = width + 0.5
                | otherwise             = x

        !y'     | y < -0.5              = -0.5
                | y > height + 0.5      = height + 0.5
                | otherwise             = y

        -- calculate discrete locations surrounding point
        !i0     = truncate (x' + 1) - 1
        !i1     = i0 + 1

        !j0     = truncate (y' + 1) - 1
        !j1     = j0 + 1

        -- calculate ratio point is between the discrete locations
        !s1     = x' - fromIntegral i0
        !s0     = 1 - s1

        !t1     = y' - fromIntegral j0
        !t0     = 1 - t1

        get' ix@(Z :. jj :. ii)
         | ii < 0        = zero
         | ii >= width'  = zero
         | jj < 0        = zero
         | jj >= height' = zero
         | otherwise     = get ix

        -- grab values from grid surrounding advected point
        !d00    = get' (Z:. j0 :. i0)
        !d01    = get' (Z:. j1 :. i0)
        !d10    = get' (Z:. j0 :. i1)
        !d11    = get' (Z:. j1 :. i1)


{-# SPECIALIZE advectElem
        :: Delta
        -> VelocityField -> (DIM2 -> Float)
        -> DIM2 -> Float #-}

{-# SPECIALIZE advectElem
        :: Delta
        -> VelocityField -> (DIM2 -> (Float,Float))
        -> DIM2  -> (Float,Float) #-}

{-
-- | Wrap an index back into the simulation area if it is outside.
checkLocation :: Float -> Float -> Float
checkLocation !width !x
   | x < 0.5          = 0.5
   | x > width - 1.5  = width - 1.5
   | otherwise        = x
{-# INLINE checkLocation #-}
-}
