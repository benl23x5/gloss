

module World
        ( eyePos
        , lights
        , objs)
where 
import Object
import Light
import Vec3

type Time = Float

-- | Position of the eye viewing the world
eyePos  = Vec3 50 (-300) (-500.0)

-- | Lights in the world
{-# NOINLINE lights #-}
lights :: [Light]
lights =
        [ LightPoint
                (Vec3 300.0 (-300.0) (-100.0))
                (Vec3 150000.0 150000.0 150000.0)

        , LightAmbient
                (Vec3 0.3 0.3 0.3) ]
                

-- | Objects in the world
{-# NOINLINE objs #-}
objs :: Time -> [Object]
objs time =
        [ Sphere 
                (Vec3 (100 * sin time) 0.0 0.0) 100.0
                (Vec3 1.0 0.3 1.0)
                0.1
                
        , Sphere
                (Vec3 180.0 0.0 0.0) 80.0
                (Vec3 0.1 0.1 1.0)
                0.5

        , Sphere
                (Vec3 (-180.0) 0.0 0.0) 80.0
                (Vec3 0.1 0.1 1.0)
                0.5

        , Sphere
                (Vec3 0.0 (-130.0) (-80.0)) 30.0
                (Vec3 1.0 1.0 1.0)
                0.8
                
        , PlaneCheck
                (Vec3 0.0 100.0 0.0)
                (normaliseV3 (Vec3 0.0 (-1.0) (-0.2)))
                0.5
        ]
