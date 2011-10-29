{-# OPTIONS -fno-warn-missing-methods #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Geometric functions concerning vectors.
module Graphics.Gloss.Data.Vector
	( Vector
	, magV
	, argV
	, dotV
	, detV
	, mulSV
	, rotateV
	, angleVV
	, normaliseV
	, unitVectorAtAngle )
where
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Angle

-- | A vector can be treated as a point, and vis-versa.
type Vector	= Point


-- | The magnitude of a vector.
magV :: Vector -> Float
{-# INLINE magV #-}
magV (x, y) 	
	= sqrt (x * x + y * y)

-- | The angle of this vector, relative to the +ve x-axis.
argV :: Vector -> Float
{-# INLINE argV #-}
argV (x, y)
	= normaliseAngle $ atan2 y x

-- | The dot product of two vectors.
dotV :: Vector -> Vector -> Float
{-# INLINE dotV #-}
dotV (x1, x2) (y1, y2)
	= x1 * y1 + x2 * y2

-- | The determinant of two vectors.
detV :: Vector -> Vector -> Float
{-# INLINE detV #-}
detV (x1, y1) (x2, y2)
	= x1 * y2 - y1 * x2

-- | Multiply a vector by a scalar.
mulSV :: Float -> Vector -> Vector
{-# INLINE mulSV #-}
mulSV s (x, y)		
	= (s * x, s * y)

-- | Rotate a vector by an angle (in radians). +ve angle is counter-clockwise.
rotateV :: Float -> Vector -> Vector
{-# INLINE rotateV #-}
rotateV r (x, y)
 = 	(  x * cos r - y * sin r
        ,  x * sin r + y * cos r)


-- | Compute the inner angle (in radians) between two vectors.
angleVV :: Vector -> Vector -> Float
{-# INLINE angleVV #-}
angleVV p1 p2
 = let 	m1	= magV p1
 	m2	= magV p2
	d	= p1 `dotV` p2
	aDiff	= acos $ d / (m1 * m2)

   in	aDiff	


-- | Normalise a vector, so it has a magnitude of 1.
normaliseV :: Vector -> Vector
{-# INLINE normaliseV #-}
normaliseV v	= mulSV (1 / magV v) v


-- | Produce a unit vector at a given angle relative to the +ve x-axis.
--	The provided angle is in radians.
unitVectorAtAngle :: Float -> Vector
{-# INLINE unitVectorAtAngle #-}
unitVectorAtAngle r
	= (cos r, sin r)

