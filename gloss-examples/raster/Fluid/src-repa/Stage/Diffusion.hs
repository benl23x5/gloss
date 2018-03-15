{-# LANGUAGE BangPatterns #-}

module Stage.Diffusion
        ( DiffSolver    (..)
        , diffusion)
where
import Model
import FieldElt
import Stage.Linear
import Data.Array.Repa          as R
import Data.Array.Repa.Eval     as R
import Data.Vector.Unboxed


data DiffSolver
        = DiffStable Int
        | DiffUnstable


-- | Diffuse a field at a certain rate.
diffusion
        :: (FieldElt a, Num a, Elt a, Unbox a)
        => DiffSolver
        -> Delta
        -> Rate
        -> Field a
        -> IO (Field a)
diffusion !solver !delta !rate field
 = {-# SCC diffusion #-}
   field `deepSeqArray`
   let  _ :. _ :. width' = R.extent field
        !width           = fromIntegral width'
   in   case solver of
         DiffUnstable
          -> let !a     = delta * rate * width * width
             in   unstableSolver field field a

         DiffStable iters
          -> let !a     = delta * rate * width * width
                 !c     = 1 + 4 * a
             in  linearSolver field field a c iters

{-# SPECIALIZE diffusion
        :: DiffSolver -> Delta -> Rate
        -> Field Float
        -> IO (Field Float) #-}

{-# SPECIALIZE diffusion
        :: DiffSolver -> Delta -> Rate
        -> Field (Float, Float)
        -> IO (Field (Float, Float)) #-}
