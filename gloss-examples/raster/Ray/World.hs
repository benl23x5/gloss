

module World
        ( makeLights
        , makeObjects)
where 
import Object
import Light
import Vec3


-- | Lights in the world
{-# NOINLINE makeLights #-}
makeLights :: Float -> [Light]
makeLights _ =
        [ Light
                (Vec3 300.0 (-300.0) (-100.0))
                (Vec3 150000.0 150000.0 150000.0) ]

                
-- | Objects in the world
{-# NOINLINE makeObjects #-}
makeObjects :: Float -> [Object]
makeObjects time =
        [ Sphere 
                (Vec3 (40 * sin time) 80 0.0)
                20
                (Vec3 1.0 0.3 1.0)
                0.4
                
        , Sphere
                (Vec3   (200 * sin time) 
                        ((-40) * sin (time + pi/2))
                        (200 * cos time))
                100.0
                (Vec3 0.4 0.4 1.0)
                0.8

        , Sphere
                (Vec3   (-200.0 * sin time) 
                        ((-40) * sin (time - pi/2))
                        (-200 * cos time))
                100.0
                (Vec3 0.4 0.4 1.0)
                0.5

        , Sphere
                (Vec3 0.0 (-150.0) (-100.0)) 50.0
                (Vec3 1.0 1.0 1.0)
                0.8
                
        , PlaneCheck
                (Vec3 0.0 100.0 0.0)
                (normaliseV3 (Vec3 0 (-1) (-0.2)))
                0.2

        ]
