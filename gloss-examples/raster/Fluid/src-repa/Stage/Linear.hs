{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Stage.Linear
        ( linearSolver
        , unstableSolver)
where
import Model
import FieldElt                         as E
import Data.Array.Repa                  as R
import Data.Array.Repa.Stencil          as R
import Data.Array.Repa.Stencil.Dim2     as R
import Data.Array.Repa.Eval             as R
import Data.Vector.Unboxed
import Prelude as P


-------------------------------------------------------------------------------
linearSolver
        :: (FieldElt a, Source U a, Unbox a, Elt a, Num a)
        => Field a      -- ^ Original field.
        -> Field a      -- ^ Current field.
        -> Float
        -> Float
        -> Int          -- ^ Number of iterations to apply.
        -> IO (Field a)

linearSolver origField curField !a !c !iters
        -- If nothing would change by running the solver, then skip it.
        | 0 <- a       = return origField

        -- The solver has finished its loop
        | 0 <- iters   = return curField

        -- Do one iteration
        | otherwise
        = origField `deepSeqArray` curField `deepSeqArray`
          do    let !c' = 1/c
                let {-# INLINE zipFunc #-}
                    zipFunc !orig !new
                        = (orig ~+~ (new ~*~ a)) ~*~ c'

                newField <- {-# SCC "linearSolver.mapStencil" #-}
                           computeUnboxedP
                         $ R.szipWith zipFunc origField
                         $ mapStencil2 (BoundConst 0) linearSolverStencil curField

                -- TODO: this boundConst thing is costing a fair bit.
                -- Do something about the branches introduced into the core code.

                linearSolver origField newField a c (iters - 1)

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
        => Stencil DIM2 a

linearSolverStencil
 = StencilStatic (Z:.3:.3) E.zero
      (\ix val acc ->
         case linearSolverCoeffs ix of
            Nothing    -> acc
            Just coeff -> acc ~+~ (val ~*~ coeff))
{-# INLINE linearSolverStencil #-}


-- | Linear solver stencil kernel.
linearSolverCoeffs :: DIM2 -> Maybe Float
linearSolverCoeffs (Z:.j:.i)
   | i ==  1, j ==  0 = Just 1
   | i == -1, j ==  0 = Just 1
   | i ==  0, j ==  1 = Just 1
   | i ==  0, j == -1 = Just 1
   | otherwise        = Nothing
{-# INLINE linearSolverCoeffs #-}


-- Unstable -------------------------------------------------------------------
unstableSolver
        :: (FieldElt a, Source U a, Unbox a, Elt a, Num a)
        => Field a      -- ^ Original field.
        -> Field a      -- ^ Current field.
        -> Float
        -> IO (Field a)

unstableSolver !origField !curField !a
 = origField `deepSeqArray` curField `deepSeqArray`
   do
        let {-# INLINE zipFunc #-}
            zipFunc !orig !new
                = orig ~+~ (new ~*~ a)

        newField
                <- computeUnboxedP
                $  R.szipWith zipFunc origField
                $  mapStencil2 (BoundConst 0) unstableSolverStencil curField

        return newField


{-# SPECIALIZE unstableSolver
        :: Field Float
        -> Field Float
        -> Float
        -> IO (Field Float) #-}

{-# SPECIALIZE unstableSolver
        :: Field (Float, Float)
        -> Field (Float, Float)
        -> Float
        -> IO (Field (Float, Float)) #-}


unstableSolverStencil
        :: FieldElt a
        => Stencil DIM2 a

unstableSolverStencil
 = StencilStatic (Z:.3:.3) E.zero
      (\ix val acc ->
         case unstableSolverCoeffs ix of
            Nothing    -> acc
            Just coeff -> acc ~+~ (val ~*~ coeff))
{-# INLINE unstableSolverStencil #-}


unstableSolverCoeffs :: DIM2 -> Maybe Float
unstableSolverCoeffs (Z:.j:.i)
    | i == 1,  j == 0   = Just 1
    | i == -1, j == 0   = Just 1
    | i == 0,  j == 1   = Just 1
    | i == 0,  j == -1  = Just 1
    | i == 0,  j == 0   = Just (-4)
    | otherwise         = Nothing
{-# INLINE unstableSolverCoeffs #-}

