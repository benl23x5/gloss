{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}

module Stage.Sources
        (addSources)
where
import Model
import FieldElt
import Data.Array.Repa          as R
import Data.Array.Repa.Unsafe   as R
import Data.Vector.Unboxed      (Unbox)


-- | Addition of forces stage for simulation
addSources
        :: (FieldSource a, Unbox a)
        => Delta                -- ^ Time delta.
        -> a                    -- ^ Value to insert.
        -> Maybe (SourceDensity a)
        -> Field a
        -> IO (Field a)

addSources !delta !value (Just (SourceDensity aim mul)) field
 = {-# SCC addSources #-}
   field `deepSeqArray`
   do   computeP $ unsafeTraverse field id (insertSource delta value aim mul)

addSources _ _ Nothing field
   = return field


insertSource
        :: (FieldSource a)
        => Delta
        -> a            -- ^ Value to insert
        -> DIM2 -> a
        -> (DIM2 -> a)
        -> DIM2
        -> a

insertSource !delta !value !aim !mul locate !pos
   | aim == pos = addSource delta value (locate pos) mul
   | otherwise  = locate pos
{-# INLINE insertSource #-}


{-# SPECIALIZE addSources
        :: Delta
        -> Float
        -> Maybe (SourceDensity Float)
        -> Field Float
        -> IO (Field Float) #-}

{-# SPECIALIZE addSources
        :: Delta
        -> (Float, Float)
        -> Maybe (SourceDensity (Float, Float))
        -> Field (Float, Float)
        -> IO (Field (Float, Float)) #-}


-- FieldSource ----------------------------------------------------------------
class FieldSource a where
        addSource :: Delta -> a -> a -> a -> a

instance FieldSource Float where
        addSource !delta !value !a !mul
         =  a ~+~ (value * delta * mul)
        {-# INLINE addSource #-}

instance FieldSource (Float, Float) where
        addSource !delta (newA, newB) (a,b) (mulA, mulB)
         = ( a + (newA * delta * (-mulA))
           , b + (newB * delta * (-mulB)))
        {-# INLINE addSource #-}




