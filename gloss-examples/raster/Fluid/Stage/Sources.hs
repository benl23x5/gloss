{-# LANGUAGE BangPatterns #-}
module Stage.Sources
        (addSources)
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

-- | Addition of forces stage for simulation
addSources 
        :: (FieldElt a, Unbox a)
        => Maybe (Source a) 
        -> Field a 
        -> IO (Field a)

addSources (Just (Source aim mul)) field
 = {-# SCC addSources #-}
   field `deepSeqArray` 
   do   traceEventIO "Fluid: addSources"
        computeP $ unsafeTraverse field id (insertSource aim mul)

addSources Nothing field
   = return field


{-# SPECIALIZE addSources 
        :: Maybe (Source Float)
        -> Field Float 
        -> IO (Field Float) #-}

{-# SPECIALIZE addSources 
        :: Maybe (Source (Float, Float))
        -> Field (Float, Float) 
        -> IO (Field (Float, Float)) #-}


insertSource 
        :: (FieldElt a) 
        => DIM2 -> a -> (DIM2 -> a) -> DIM2 -> a
insertSource !aim !mul locate !pos
   | aim == pos = addSource (locate pos) mul
   | otherwise  = locate pos
{-# INLINE insertSource #-}
