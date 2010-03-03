{-# OPTIONS -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Utils concerning Vectors.
module Graphics.Gloss.Geometry.Vector
	( magnitude
	, arg
	, dot
	, det
	, mulSV
	, rotateV
	, rotateV_deg
	, angleVV
	, normaliseV
	, unitVectorAtAngle )
where
import Graphics.Gloss.Picture		(Vector)
import Graphics.Gloss.Geometry.Angle


-- | Pretend a vector is a number.
--	Vectors aren't real numbes according to Haskell, because they don't
--	support the multiply and divide field operators. We can pretend they
--	are though, and use the (+) and (-) operators as component-wise
--	addition and subtraction.
--
instance Num (Float, Float) where
 	(+) (x1, y1) (x2, y2)	= (x1 + x2, y1 + y2)
 	(-) (x1, y1) (x2, y2)	= (x1 - x2, y1 - y2)
	negate (x, y)		= (negate x, negate y)	


-- | Work out the magnitude of a vector
{-# INLINE magnitude #-}
magnitude :: Vector -> Float
magnitude (x, y) 	
	= sqrt (x * x + y * y)

-- | The angle of this vector
arg :: Vector -> Float
arg (x, y)
	= normaliseAngle $ atan2 y x

-- | The dot product for vectors
{-# INLINE dot #-}
dot :: Vector -> Vector -> Float
dot (x1, x2) (y1, y2)
	= x1 * y1 + x2 * y2

-- | The determinant of two vectors
{-# INLINE det #-}
det :: Vector -> Vector -> Float
det (x1, y1) (x2, y2)
	= x1 * y2 - y1 * x2

{-# INLINE mulSV #-}
-- | Multiply a vector by a scalar
mulSV :: Float -> Vector -> Vector
mulSV s (x, y)		
	= (s * x, s * y)

-- | Rotate a vector by an angle (radians)
--	+ve is counter clockwise
{-# INLINE rotateV #-}
rotateV :: Float -> Vector -> Vector
rotateV r (x, y)
 = 	(  x * cos r - y * sin r
        ,  x * sin r + y * cos r)

-- | Rotate a vector by a number of degrees
{-# INLINE rotateV_deg #-}
rotateV_deg :: Float -> Vector -> Vector
rotateV_deg d (x, y)
 = let	r	= degToRad d
   in	(  x * cos r - y * sin r
        ,  x * sin r + y * cos r)

-- | Compute the inner angle between two vectors
{-# INLINE angleVV #-}
angleVV :: Vector -> Vector -> Float
angleVV p1@(x1, y1) p2@(x2, y2)
 = let 	m1	= magnitude p1
 	m2	= magnitude p2
	d	= p1 `dot` p2
	aDiff	= acos $ d / (m1 * m2)

   in	aDiff	


-- | normalise a vector
{-# INLINE normaliseV #-}
normaliseV :: Vector -> Vector
normaliseV v	= mulSV (1 / magnitude v) v

-- | the normal vector at an angle (in radians)
{-# INLINE unitVectorAtAngle #-}
unitVectorAtAngle :: Float -> Vector
unitVectorAtAngle r
	= (cos r, sin r)
