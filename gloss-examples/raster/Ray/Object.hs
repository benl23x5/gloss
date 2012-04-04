{-# LANGUAGE BangPatterns #-}

module Object 
        ( Color
        , Object(..)
        , translateObject
        , castRay
        , castRay_continuation
        , checkRay
        , surfaceNormal
        , colorOfObject
        , shineOfObject)
where
import Vec3

type Color      = Vec3

-- | An object in the world
data Object
        = Sphere
        { spherePos             :: !Vec3
        , sphereRadius          :: !Float
        , sphereColor           :: !Color
        , sphereShine           :: !Float }

        | Plane
        { planePos              :: !Vec3
        , planeNormal           :: !Vec3
        , planeColor            :: !Color
        , planeShine            :: !Float }

        | PlaneCheck
        { planeCheckPos         :: !Vec3
        , planeCheckNormal      :: !Vec3
        , planeCheckShine       :: !Float }
        deriving (Eq, Show)


translateObject :: Vec3 -> Object -> Object
translateObject v obj
 = case obj of
        Sphere{}        -> obj { spherePos     = spherePos     obj + v }
        Plane{}         -> obj { planePos      = planePos      obj + v }
        PlaneCheck{}    -> obj { planeCheckPos = planeCheckPos obj + v }
{-# INLINE translateObject #-}


-- | Find the nearest point of intersection for a ray
castRay :: [Object]        -- check for intersections on all these objects
        -> Vec3            -- ray origin
        -> Vec3            -- ray direction
        -> Maybe 
                ( Object   -- object of first intersected
                , Vec3)    -- position of intersection, on surface of object

castRay !objs !orig !dir
 = go0 objs
 where -- We haven't hit any objects yet.
       go0 []     = Nothing
       go0 (obj:rest) 
        = case distanceToObject obj orig dir of
           Nothing    -> go0 rest
           Just dist  -> go1 rest obj dist

       -- We hit an object before, and we're testing others
       -- to see if they're closer.
       go1 []         !objClose !dist 
        = Just (objClose, orig + dir `mulsV3` dist)

       go1 (obj:rest) !objClose !dist
        = case distanceToObject obj orig dir of
           Nothing         -> go1 rest objClose dist
           Just dist'
            | dist' < dist -> go1 rest obj      dist'
            | otherwise    -> go1 rest objClose dist
{-# INLINE castRay #-}


-- | Like castRay, but take continuations for the Nothing and Just branches to 
--   eliminate intermediate unboxings.
castRay_continuation
        :: [Object]        -- check for intersections on all these objects
        -> Vec3            -- ray origin
        -> Vec3            -- ray direction

        -> a                     -- continuation when no intersection 
        -> (Object -> Vec3 -> a) -- continuation with intersection
        -> a

castRay_continuation !objs !orig !dir contNone contJust
 = go0 objs
 where -- We haven't hit any objects yet.
       go0 []     = contNone
       go0 (obj:rest) 
        = case distanceToObject obj orig dir of
           Nothing    -> go0 rest
           Just dist  -> go1 rest obj dist

       -- We hit an object before, and we're testing others
       -- to see if they're closer.
       go1 []         !objClose !dist 
        = contJust objClose (orig + dir `mulsV3` dist)

       go1 (obj:rest) !objClose !dist
        = case distanceToObject obj orig dir of
           Nothing         -> go1 rest objClose dist
           Just dist'
            | dist' < dist -> go1 rest obj      dist'
            | otherwise    -> go1 rest objClose dist
{-# INLINE castRay_continuation #-}


-- | Simplified version of `castRay` that only checks whether there is some
--   object closer than a given mimimum distance.
checkRay :: [Object]    -- ^ Check for intersection on all these objects.
         -> Vec3        -- ^ Ray origin.
         -> Vec3        -- ^ Ray direction.
         -> Float       -- ^ Minimum distance.
         -> Bool
        
checkRay !objs !orig !dir !dist
 = go0 objs
 where  go0 []          = False
        go0 (obj:rest)
         = case distanceToObject obj orig dir of
            Nothing             -> go0 rest
            Just dist'
             | dist' < dist     -> True
             | otherwise        -> go0 rest
{-# INLINE checkRay #-}             


-- | Compute the distance to the surface of this shape
distanceToObject
        :: Object       -- ^ Towards this object.
        -> Vec3         -- ^ Start from this point.
        -> Vec3         -- ^ Along this ray.
        -> Maybe Float  -- ^ Distance to intersection, if there is one.

distanceToObject !obj !orig !dir
 = case obj of
    Sphere pos radius _ _
     -> let !p       = orig + dir `mulsV3` ((pos - orig) `dotV3` dir) 
            !d_cp    = magnitudeV3 (p - pos)
        in  if    d_cp >= radius                  then Nothing
            else if (p - orig) `dotV3` dir <= 0.0 then Nothing
            else Just $ magnitudeV3 (p - orig) - sqrt (radius * radius - d_cp * d_cp)

    Plane pos normal _ _
     -> if dotV3 dir normal >= 0.0 
                then Nothing
                else Just (((pos - orig) `dotV3` normal) / (dir `dotV3` normal))

    PlaneCheck pos normal _
     -> if dotV3 dir normal >= 0.0 
                then Nothing
                else Just (((pos - orig) `dotV3` normal) / (dir `dotV3` normal))
{-# INLINE distanceToObject #-}

                
-- | Compute the surface normal of the shape at this point
surfaceNormal   
        :: Object
        -> Vec3         -- ^ A point on the surface of the shape.
        -> Vec3

surfaceNormal obj point
 = case obj of
    Sphere     pos _ _ _    -> normaliseV3 (point - pos)
    Plane      _ normal _ _ -> normal
    PlaneCheck _ normal _   -> normal
{-# INLINE surfaceNormal #-}


-- | Get the color of an object at the given point.
colorOfObject :: Object -> Vec3 -> Color
colorOfObject obj point
 = case obj of
        Sphere _ _ c _   -> c
        Plane  _ _ c _   -> c
        PlaneCheck{}     -> checkers point
{-# INLINE colorOfObject #-}


-- | Get the shine of an object at the given point.
shineOfObject :: Object -> Vec3 -> Float
shineOfObject obj _point
 = case obj of 
        Sphere _ _ _ s   -> s
        Plane  _ _ _ s   -> s
        PlaneCheck _ _ s -> s
{-# INLINE shineOfObject #-}

                
-- | A checkerboard pattern along the x/z coords
checkers :: Vec3 -> Vec3
checkers (Vec3 x _ z)
        |       ((truncate (z / 100.0) :: Int)`mod` 2 == 0)
          `xor` ((truncate (x / 100.0) :: Int) `mod` 2 == 0)
          `xor` (x < 0.0)
          `xor` (z < 0.0)
        = Vec3 1.0 1.0 1.0
        
        | otherwise
        = Vec3 0.4 0.4 0.4


xor :: Bool -> Bool -> Bool
xor x1 x2
 = case (x1, x2) of
        (False, False)  -> False
        (False, True)   -> True
        (True,  False)  -> True
        (True,  True)   -> False
{-# INLINE xor #-}
