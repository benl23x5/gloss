-- | Geometric functions concerning angles. If not otherwise specified, all angles are in radians.
module Graphics.Gloss.Geometry.Angle
	( degToRad
	, radToDeg
	, normalizeAngle )
where

-- | Convert degrees to radians
degToRad :: Float -> Float
degToRad d	= d * pi / 180
{-# INLINE degToRad #-}


-- | Convert radians to degrees
radToDeg :: Float -> Float
radToDeg r	= r * 180 / pi
{-# INLINE radToDeg #-}


-- | Normalize an angle to be between 0 and 2*pi radians
normalizeAngle :: Float -> Float
normalizeAngle f = f - 2 * pi * floor' (f / (2 * pi))
 where  floor' :: Float -> Float
        floor' x = fromIntegral (floor x :: Int)
{-# INLINE normalizeAngle #-}
