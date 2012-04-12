{-# LANGUAGE BangPatterns, FlexibleInstances #-}
module Stage.Sources
        (addSources)
where
import Model
import FieldElt
import Constants
import Data.Array.Repa          as R
import Data.Array.Repa.Unsafe   as R
import Data.Vector.Unboxed      (Unbox)
import Debug.Trace
import Data.IORef
import System.IO.Unsafe


-- | Addition of forces stage for simulation
addSources 
        :: (FieldElt a, FieldSource a, Unbox a)
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
        :: (FieldElt a, FieldSource a) 
        => DIM2 -> a -> (DIM2 -> a) -> DIM2 -> a
insertSource !aim !mul locate !pos
   | aim == pos = addSource (locate pos) mul
   | otherwise  = locate pos
{-# INLINE insertSource #-}


class FieldSource a where
        addSource :: a    -> a     -> a


instance FieldSource Float where
        addSource a mul 
         = let  !density        = unsafePerformIO $ readIORef densArg
                !dt             = unsafePerformIO $ readIORef dtArg
           in   a ~+~ (density * dt * mul)
        {-# INLINE addSource #-}


instance FieldSource (Float, Float) where
        addSource (a,b) (mulA, mulB)
         = let  !velocity       = unsafePerformIO $ readIORef velArg
                !dt             = unsafePerformIO $ readIORef dtArg
                (newA, newB)    = velocity
           in  ( a + (newA * dt * (-mulA))
               , b + (newB*dt*(-mulB)))
        {-# INLINE addSource #-}
