{-# LANGUAGE BangPatterns #-}
module Stage.Diffusion
        (diffusion)
where
import Model
import FieldElt
import Stage.Linear
import Data.Array.Repa          as R
import Data.Array.Repa.Eval     as R
import Data.Vector.Unboxed
import Debug.Trace


-- | Diffuse a field at a certain rate.
diffusion 
        :: (FieldElt a, Num a, Elt a, Unbox a) 
        => Delta -> Rate
        -> Field a 
        -> IO (Field a)
diffusion !delta !rate field 
 = {-# SCC diffusion #-}
   field `deepSeqArray`  
   let  _ :. _ :. width' = R.extent field
        !width           = fromIntegral width'
        !a               = delta * rate * width * width
        !c               = 1 + 4 * a
        !repeats         = 20
   in do
        traceEventIO "Fluid: diffusion"
        linearSolver field field a c repeats

{-# SPECIALIZE diffusion 
        :: Delta -> Rate
        -> Field Float 
        -> IO (Field Float) #-}

{-# SPECIALIZE diffusion 
        :: Delta -> Rate
        -> Field (Float, Float) 
        -> IO (Field (Float, Float)) #-}
