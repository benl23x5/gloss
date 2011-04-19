
{-# LANGUAGE BangPatterns #-}
module Vec2 where

data Vec2 
        = Vec2 {-# UNPACK #-}!Double {-# UNPACK #-}!Double
        deriving Show


vecZero :: Vec2
vecZero = Vec2 0.0 0.0


vecAdd :: Vec2 -> Vec2 -> Vec2
vecAdd (Vec2 a b) (Vec2 x y)
        = Vec2 (a+x) (b+y)


vecSub :: Vec2 -> Vec2 -> Vec2
vecSub (Vec2 a b) (Vec2 x y)
        = Vec2 (a-x) (b-y)


vecScale :: Vec2 -> Double -> Vec2
vecScale (Vec2 a b) !s
        = Vec2 (a*s) (b*s)


vecDot :: Vec2 -> Vec2 -> Double
vecDot (Vec2 a b) (Vec2 x y)
        = (a*x)+(b*y)


vecNorm :: Vec2 -> Double
vecNorm v
        = sqrt (vecDot v v)


vecNormalize :: Vec2 -> Vec2
vecNormalize v
        = vecScale v (1.0 / (vecNorm v))


vecDimSelect :: Vec2 -> Int -> Double
vecDimSelect (Vec2 a b) n
 = case rem n 2 of
      0 -> a
      1 -> b


vecLessThan :: Vec2 -> Vec2 -> Bool
vecLessThan (Vec2 a b) (Vec2 x y)
        = a < x && b < y


vecGreaterThan :: Vec2 -> Vec2 -> Bool
vecGreaterThan (Vec2 a b) (Vec2 x y)
        = a > x && b > y

