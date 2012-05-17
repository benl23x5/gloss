{-# LANGUAGE BangPatterns #-}
module Stage.Project
        (project)
where
import Model
import FieldElt
import Stage.Linear
import Data.Array.Repa          as R
import Data.Array.Repa.Unsafe   as R
import Debug.Trace
import Prelude                  as P


project :: Int -> Field (Float, Float) -> IO (Field (Float, Float))
project iters field
 = {-# SCC project #-}
   field `deepSeqArray` 
   do   let _ :. _ :. width = extent field

        divergence <- {-# SCC "project.genDiv" #-}
                      computeUnboxedP 
                   $  fromFunction (Z:. width :. width) (genDivergence width field)

        p          <- {-# SCC "project.linearSolver" #-}
                      linearSolver divergence divergence 1 4 iters

        f'         <- {-# SCC "project.apply" #-}
                      computeUnboxedP 
                $     unsafeTraverse field id (projectElem width p)

        return f'
{-# NOINLINE project #-}


-- | Subtract a gradient field from the regular field to 
--   create a mass-conserving field.
projectElem
        :: Int                          -- ^ Width of model.
        -> Field Float
        -> (DIM2 -> (Float, Float))     -- ^ Get data from the regular field.
        -> DIM2                         -- ^ Compute the value at this point.
        -> (Float, Float)

projectElem !width !p !get !pos@(Z:.j:.i)
 = get pos ~-~ (0.5 * width' * (p0 - p1),
                0.5 * width' * (p2 - p3))
 where
        !width' = fromIntegral width
        !p0     = useIf (i < width - 1) (p `unsafeIndex` (Z :. j   :. i+1))
        !p1     = useIf (i >         0) (p `unsafeIndex` (Z :. j   :. i-1))
        !p2     = useIf (j < width - 1) (p `unsafeIndex` (Z :. j+1 :. i  ))
        !p3     = useIf (j >         0) (p `unsafeIndex` (Z :. j-1 :. i  ))
{-# INLINE projectElem #-}


-- | Get an approximation of the gradient at this point.
genDivergence :: Int -> VelocityField -> DIM2 -> Float
genDivergence !width !f (Z :. j :. i)
 = (-0.5 * ((u0 - u1) + (v0 - v1))) / fromIntegral width
 where
      (u0,  _) = useIf (i < width - 1) (f `unsafeIndex` (Z:. j   :. i+1))
      (u1,  _) = useIf (i >         0) (f `unsafeIndex` (Z:. j   :. i-1))
      ( _, v0) = useIf (j < width - 1) (f `unsafeIndex` (Z:. j+1 :. i  ))
      ( _, v1) = useIf (j >         0) (f `unsafeIndex` (Z:. j-1 :. i  ))
{-# INLINE genDivergence #-}


dumpArray :: Array U DIM2 Float -> IO ()
dumpArray arr
 = do   let (Z :. h :. w) = extent arr
        let vals          = R.toList arr
        let mx            = P.maximum vals

        let getVal x y
             =  arr R.! (Z :. y :. x)

        let showVal x y
             =  let  v   = getVal x y
                in P.replicate (15 - P.length (show v)) ' ' P.++ show v

        let out = unlines $ 
                [ P.concat [ showVal x y P.++ " "
                              | x <- [0..w - 1]]
                     | y <- [h - 1, h - 2 .. 0]]

        putStrLn out
