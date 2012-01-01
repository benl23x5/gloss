

module World
        ( eyePos
        , lights
        , objs
        , checkers)
where 
import Object
import Light
import Vec3

type Time = Float

-- | Position of the eye viewing the world
eyePos  = Vec3 1.0 0.0 (-500.0)

-- | Lights in the world
lights =
        [ LightPoint
                (Vec3 300.0 (-300.0) (-100.0))
                (Vec3 150000.0 150000.0 150000.0)

        , LightAmbient
                (Vec3 0.3 0.3 0.3) ]
                

-- | Objects in the world
objs :: Time -> [Object]
objs time =
        [ Object 
                (Sphere (Vec3 (100 * sin time) 0.0 0.0) 100.0)
                (const $ Vec3 1.0 0.3 1.0)
                (const 0.1)
                
        , Object 
                (Sphere (Vec3 180.0 0.0 0.0) 80.0)
                (const $ Vec3 0.1 0.1 1.0)
                (const 0.5) 

        , Object 
                (Sphere (Vec3 (-180.0) 0.0 0.0) 80.0)
                (const $ Vec3 0.1 0.1 1.0)
                (const 0.5) 

        , Object 
                (Sphere (Vec3 0.0 (-130.0) (-80.0)) 30.0)
                (const $ Vec3 1.0 1.0 1.0)
                (const 0.8) 
                
        , Object 
                (Plane  (Vec3 0.0 100.0 0.0) 
                        (normaliseV3 (Vec3 0.0 (-1.0) (-0.2))))
                checkers
                (const 0.5)  ]
                
-- | A checkerboard pattern along the x/z coords
checkers (Vec3 x y z)
        |       (truncate (z / 100.0) `mod` 2 == 0)
          `xor` (truncate (x / 100.0) `mod` 2 == 0)
          `xor` (x < 0.0)
          `xor` (z < 0.0)
        = Vec3 1.0 1.0 1.0
        
        | otherwise
        = Vec3 0.2 0.2 0.2


xor :: Bool -> Bool -> Bool
xor x1 x2
 = case (x1, x2) of
        (False, False)  -> False
        (False, True)   -> True
        (True,  False)  -> True
        (True,  True)   -> False
{-# INLINE xor #-}
