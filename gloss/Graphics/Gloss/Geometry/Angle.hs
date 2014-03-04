-- | Geometric functions concerning angles. If not otherwise specified, all angles are in radians.
module Graphics.Gloss.Geometry.Angle
	( degToRad
	, radToDeg
	, normaliseAngle )
where

-- | Convert degrees to radians
{-# INLINE degToRad #-}
degToRad :: Float -> Float
degToRad d	= d * pi / 180


-- | Convert radians to degrees
{-# INLINE radToDeg #-}
radToDeg :: Float -> Float
radToDeg r	= r * 180 / pi


-- | Normalise an angle to be between 0 and 2*pi radians
{-# INLINE normaliseAngle #-}
normaliseAngle :: Float -> Float
normaliseAngle f = f - 2 * pi * floor' (f / (2 * pi))
 where  floor' :: Float -> Float
        floor' x = fromIntegral (floor x :: Int)
