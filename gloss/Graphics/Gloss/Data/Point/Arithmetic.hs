{-# LANGUAGE BangPatterns #-}

-- |
-- == Point and vector arithmetic
--
-- Vectors aren't numbers according to Haskell, because they don't
-- support all numeric operations sensibly. We define component-wise
-- addition, subtraction, and negation along with scalar multiplication
-- in this module, which is intended to be imported qualified.
module Graphics.Gloss.Data.Point.Arithmetic
  (
    Point
  , (+)
  , (-)
  , (*)
  , negate
  ) where
import Prelude (Float)
import qualified Prelude as P
import Graphics.Gloss.Rendering (Point)

infixl 6 +, -
infixl 7 *

-- | Add two vectors, or add a vector to a point.
(+) :: Point -> Point -> Point
(x1, y1) + (x2, y2) =
  let
    !x = x1 P.+ x2
    !y = y1 P.+ y2
  in (x, y)

-- | Subtract two vectors, or subtract a vector from a point.
(-) :: Point -> Point -> Point
(x1, y1) - (x2, y2) =
  let
    !x = x1 P.- x2
    !y = y1 P.- y2
  in (x, y)

-- | Negate a vector.
negate :: Point -> Point
negate (x, y) =
  let
    !x' = P.negate x
    !y' = P.negate y
  in (x', y')

-- | Multiply a scalar by a vector.
(*) :: Float -> Point -> Point
(*) s (x, y) =
  let
    !x' = s P.* x
    !y' = s P.* y
  in (x', y')
