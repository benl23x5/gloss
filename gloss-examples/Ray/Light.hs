{-# LANGUAGE BangPatterns #-}

module Light 
        ( Light(..)
        , applyLight)
where
import Object
import Vec3


-- | A primitive light
data Light
        -- | A point light source, intensity drops off with distance from the point.
        = LightPoint 
        { point   :: Vec3
        , color   :: Color }

        -- | Ambient light source, applied to all objects all the time.
        | LightAmbient 
        { color   :: Color }


-- | Compute the direct lighting at a particular point
applyLight
        :: [Object]     -- possible occluding objects, used for shadows.
        -> Vec3         -- point which is being lit
        -> Vec3         -- surface normal at this point
        -> Light 
        -> Color

applyLight objs pt n (LightPoint lpt color)
 = let
        -- vector from the light to the surface point
        !dir     = normaliseV3 (lpt - pt)

        -- distance from light source to surface
        !dist    = magnitudeV3 (lpt - pt)
        
        -- magnitude of reflection
        !mag     = (n `dotV3` dir) / (dist * dist)

        -- eliminate negative lights
        !final   = clampV3 (color `mulsV3` mag) 0.0 999999.0
        
        -- check for occluding objects between the light and the surface point
   in   case castRay objs pt dir of
                Just (_, opt)
                 -> if magnitudeV3 (opt - pt) < dist
                        then Vec3 0.0 0.0 0.0
                        else final 
                        
                Nothing -> final
                 
applyLight opt pt n (LightAmbient color)
        = color
