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
        -> Field a      -- ^ Current field.
        -> Float
        -> Float
        -> Int          -- ^ Number of iterations to apply.
        -> IO (Field a)

linearSolver origField curField !a !c !i
        -- If nothing would change by running the solver, then skip it.
        | 0 <- a       = return origField

        -- The solver has finished its loop
        | 0 <- i       = return curField

        -- Do one iteration
        | otherwise
        = origField `deepSeqArray` curField `deepSeqArray`
          do    let !c' = 1/c
                let {-# INLINE zipFunc #-}
                    zipFunc !orig !new
                        = new ~+~ (orig ~*~ c')

                traceEventIO "Fluid: linear solver mapStencil"
                newField <- {-# SCC "linearSolver.mapStencil" #-}
                           computeUnboxedP 
                         $ R.czipWith zipFunc origField
                         $ mapStencil2 (BoundConst E.zero) (linearSolverStencil a c) curField

                linearSolver origField newField a c (i - 1)

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


-- | Stencil function for the linear solver.
linearSolverStencil 
        :: FieldElt a
        => Float -> Float -> Stencil DIM2 a

linearSolverStencil a c 
 = StencilStatic (Z:.3:.3) E.zero
      (\ix val acc ->
         case linearSolverCoeffs a c ix of
            Nothing    -> acc
            Just coeff -> acc ~+~ (val ~*~ coeff))
{-# INLINE linearSolverStencil #-}
         

-- | Linear solver stencil kernel.
linearSolverCoeffs 
        :: Float -> Float -> DIM2 -> Maybe Float

linearSolverCoeffs a c (Z:.j:.i)
   | i ==  1, j ==  0 = Just (a/c)
   | i == -1, j ==  0 = Just (a/c)
   | i ==  0, j ==  1 = Just (a/c)
   | i ==  0, j == -1 = Just (a/c)
   | otherwise        = Nothing
{-# INLINE linearSolverCoeffs #-}

