{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Stage.Linear
        (linearSolver)
where
import Model
import FieldElt                         as E
import Data.Array.Repa                  as R
import Data.Array.Repa.Stencil          as R
import Data.Array.Repa.Stencil.Dim2     as R
import Data.Array.Repa.Eval             as R
import Data.Vector.Unboxed
import Debug.Trace


linearSolver 
        :: (FieldElt a, Repr U a, Unbox a, Elt a, Num a)
        => Field a      -- ^ Original field.
        -> Field a      -- ^ current field.
        -> Float        -- 
        -> Float
        -> Int          -- ^ Number of iterations to apply.
        -> IO (Field a)

-- If linearSolver would not actually change anything by running, skip
linearSolver origF _  !0  !_  !_
        = return origF

-- If linearSolver has finished loop
linearSolver _     f  !_  !_  !0
        = return f

-- Calculate intermediate array
linearSolver origF f !a !c !i
 = origF `deepSeqArray` f `deepSeqArray`
   do   
        let !c' = 1/c
        let {-# INLINE zipFunc #-}
            zipFunc !orig !new
                = new ~+~ (orig ~*~ c')

        traceEventIO "Fluid: linear solver mapStencil"
        f'      <- {-# SCC "linearSolver.mapStencil" #-}
                   computeUnboxedP 
                 $ R.czipWith zipFunc origF
                 $ mapStencil2 (BoundConst E.zero) (linearSolverStencil a c) f

{-
        traceEventIO "Fluid: linear solver zipWith"
        f''     <- {-# SCC "linearSolver.zipWith" #-}
                   computeUnboxedP $ R.zipWith zipFunc origF f'
-}
        linearSolver origF f' a c (i - 1)

{-# SPECIALIZE linearSolver 
        :: Field Float 
        -> Field Float 
        -> Float -> Float -> Int 
        -> IO (Field Float) #-}

{-# SPECIALIZE linearSolver 
        :: Field (Float, Float) 
        -> Field (Float, Float) 
        -> Float -> Float -> Int 
        -> IO (Field (Float, Float)) #-}



-- Function is specialised, rather than inlined, as GHC would not inline the function
-- therwise
-- zipFunc :: (FieldElt a) => Float -> a -> a -> a
-- zipFunc !c !orig !new = new ~+~ (orig ~*~ c)
-- {-# INLINE zipFunc #-}



linearSolverStencil 
        :: FieldElt a
        => Float -> Float
        -> Stencil DIM2 a

linearSolverStencil a c 
 = StencilStatic (Z:.3:.3) E.zero
      (\ix val acc ->
         case linearSolverCoeffs a c ix of
            Nothing -> acc
            Just coeff -> acc ~+~ (val ~*~ coeff))
{-# INLINE linearSolverStencil #-}
         

linearSolverCoeffs 
        :: Float -> Float
        -> DIM2  
        -> Maybe Float
linearSolverCoeffs a c (Z:.j:.i)
   | i ==  1, j ==  0 = Just (a/c)
   | i == -1, j ==  0 = Just (a/c)
   | i ==  0, j ==  1 = Just (a/c)
   | i ==  0, j == -1 = Just (a/c)
   | otherwise        = Nothing
{-# INLINE linearSolverCoeffs #-}

