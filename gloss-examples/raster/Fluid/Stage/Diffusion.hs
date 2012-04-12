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
import Debug.Trace
import System.IO.Unsafe
import Data.IORef

diffusion 
        :: (FieldElt a, Num a, Elt a, Unbox a) 
        => Field a -> Float -> IO (Field a)
diffusion f !rate
 = {-# SCC diffusion #-}
   f `deepSeqArray`  
   do   let !width   = fromIntegral $ unsafePerformIO $ readIORef widthArg

        let !a       = dt * rate * width * width
        let !c       = 1 + 4 * a
        let !repeats = 20
        traceEventIO "Fluid: diffusion"
        linearSolver f f a c repeats

{-# SPECIALIZE diffusion 
        :: Field Float -> Float 
        -> IO (Field Float) #-}

{-# SPECIALIZE diffusion 
        :: Field (Float, Float) -> Float 
        -> IO (Field (Float, Float)) #-}
