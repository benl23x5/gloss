{-# LANGUAGE BangPatterns #-}

module Object where
import Vec3
import Data.List
import Data.Function
import Data.Maybe


type Color      = Vec3


-- | An object in the world
data Object
        = Object 
        { objectShape   :: Shape

          -- diffuse color of the object
        , objectColor   :: Vec3 -> Color

          -- amount of reflectance, [0..1]
        , objectShine   :: Vec3 -> Float }


-- | Find the nearest point of intersection for a ray
findNearest 
        :: [Object]        -- check for intersections on all these objects
        -> Vec3            -- ray origin
        -> Vec3            -- ray direction
        -> Maybe 
                ( Object   -- object of first intersected
                , Vec3)    -- position of intersection, on surface of object

findNearest objs orig dir
 = let  result  = mapMaybe (consider orig dir) objs   
   in   case result of
         []     -> Nothing

         _      -> let (obj, d)        = minimumBy (compare `on` snd) result 
                   in  Just (obj, orig + dir `mulsV3` d)
                

-- | Check if a ray intersects this object
consider 
        :: Vec3             -- origin of ray
        -> Vec3             -- direction of ray
        -> Object           -- object to test
        -> Maybe ( Object   -- the object that intersected
                 , Float )  -- distance of intersection point from origin

consider orig dir obj
 = case distanceToShape (objectShape obj) orig dir of
        Nothing -> Nothing
        Just d  -> Just (obj, d)


-- | A primitive shape in the scene
data Shape
        -- | A sphere with a center and radius.
        = Sphere !Vec3 !Float

        -- | A plane with a point and normal vector.
        | Plane  !Vec3 !Vec3

 
-- | Compute the distance to the surface of this shape
distanceToShape
        :: Shape        -- ^ Towards this shape.
        -> Vec3         -- ^ Start from this point.
        -> Vec3         -- ^ Along this ray.
        -> Maybe Float -- ^ Distance to intersection, if there is one.

distanceToShape shape orig dir
 = case shape of
    Sphere c r
     -> let !p       = orig + dir `mulsV3` ((c - orig) `dotV3` dir) 
            !d_cp    = magnitudeV3 (p - c)
            !d       = magnitudeV3 (p - orig) - sqrt (r * r - d_cp * d_cp)
            
        in  if      d_cp >= r                     then Nothing
            else if (p - orig) `dotV3` dir <= 0.0 then Nothing
            else Just d

    Plane p n
     -> if dotV3 dir n >= 0.0 
                then Nothing
                else Just (((p - orig) `dotV3` n) / (dir `dotV3` n))

                
-- | Compute the surface normal of the shape at this point
surfaceNormal   
        :: Shape        
        -> Vec3         -- ^ A point on the surface of the shape.
        -> Vec3

surfaceNormal shape point
 = case shape of
    Sphere c r  -> normaliseV3 (point - c)
    Plane  p n  -> n
