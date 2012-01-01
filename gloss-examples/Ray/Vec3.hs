{-# OPTIONS -fno-warn-missing-methods #-}

module Vec3
        ( Vec3(..)
        , magnitudeV3
        , normaliseV3
        , mulsV3
        , dotV3
        , clampV3
        , clipV3)
where


data Vec3
        = Vec3 !Float !Float !Float
        deriving (Eq, Show)


instance Num Vec3 where
 (+) (Vec3 x1 x2 x3) (Vec3 y1 y2 y3)
        = Vec3 (x1 + y1) (x2 + y2) (x3 + y3)
 {-# INLINE (+) #-}

 (-) (Vec3 x1 x2 x3) (Vec3 y1 y2 y3)
        = Vec3 (x1 - y1) (x2 - y2) (x3 - y3)
 {-# INLINE (-) #-}

 (*) (Vec3 x1 x2 x3) (Vec3 y1 y2 y3)
        = Vec3 (x1 * y1) (x2 * y2) (x3 * y3)
 {-# INLINE (*) #-}


-- | Yield the magnitude of a vector.
magnitudeV3 :: Vec3 -> Float
magnitudeV3 (Vec3 x y z)
        = sqrt (x * x + y * y + z * z)
{-# INLINE magnitudeV3 #-}


-- | Normalise a vector to have unit length.
normaliseV3 :: Vec3 -> Vec3
normaliseV3 v
        = v `mulsV3` (1.0 / magnitudeV3 v) 
{-# INLINE normaliseV3 #-}


-- | Multiply a vector by a scalar.
mulsV3 :: Vec3 -> Float -> Vec3 
mulsV3 (Vec3 x1 y1 z1) s
        = Vec3 (s * x1) (s * y1) (s * z1)
{-# INLINE mulsV3 #-}


-- | Compute the dot product of two vectors.
dotV3 :: Vec3 -> Vec3 -> Float
dotV3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2)
        = x1 * x2 + y1 * y2 + z1 * z2
{-# INLINE dotV3 #-}


-- | Clamp a vectors components to some minimum and maximum values.
clampV3 :: Vec3 -> Float -> Float -> Vec3
clampV3 (Vec3 r g b) minVal maxVal
 = Vec3 (clamp r) (clamp g) (clamp b)
 where {-# INLINE clamp #-}
       clamp x 
        | x <= minVal   = minVal
        | x >= maxVal   = maxVal
        | otherwise     = x
{-# INLINE clampV3 #-}


-- | Clip a vector's components to some maxiumum value.
clipV3 :: Vec3 -> Float -> Vec3
clipV3 (Vec3 r g b) maxVal
 = Vec3 (clip r) (clip g) (clip b)
 where {-# INLINE clip #-}
       clip x
        | x > maxVal    = maxVal
        | otherwise     = x
{-# INLINE clipV3 #-}
