{-# LANGUAGE BangPatterns #-}
module Stage.Diffusion
        (diffusion)
where
import Model
import FieldElt
import Constants
import Stage.Linear
import Data.Array.Repa          as R
import Data.Array.Repa.Eval     as R
import Data.Vector.Unboxed      

diffusion 
        :: (FieldElt a, Num a, Elt a, Unbox a) 
        => Field a -> Float -> IO (Field a)
diffusion f !rate
 = f `deepSeqArray`  
   do   let !a       = dt * rate * widthF * widthF
        let !c       = 1 + 4 * a
        let !repeats = 20
        linearSolver f f a c repeats

{-# SPECIALIZE diffusion 
        :: Field Float -> Float 
        -> IO (Field Float) #-}

{-# SPECIALIZE diffusion 
        :: Field (Float, Float) -> Float 
        -> IO (Field (Float, Float)) #-}
