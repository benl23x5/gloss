{-# LANGUAGE BangPatterns #-}

module Light 
        ( Light(..)
        , translateLight
        , applyLights)
where
import Object
import Vec3


-- | A primitive light
data Light
        -- | A point light source, intensity drops off with distance from the point.
        = Light
        { lightPoint   :: !Vec3
        , lightColor   :: !Color }
        deriving (Eq, Show)

translateLight :: Vec3 -> Light -> Light
translateLight v ll
 = case ll of
        Light pos color -> Light (pos + v) color
{-# INLINE translateLight #-}


-- | Compute the direct lighting at particular point for a list of lights.
applyLights
        :: [Object]     -- ^ Possible occluding objects, used for shadows.
        -> Vec3         -- ^ Point which is being lit.
        -> Vec3         -- ^ Surface normal at this point.
        -> [Light]      -- ^ Lights to consider.
        -> Color        -- ^ Total lighting at this point.

applyLights !objs !point !normal !lights
 = go lights (Vec3 0 0 0)
 where go [] !total     = total
       go (light:rest) !total
        = let !contrib  = applyLight objs point normal light
          in  go rest (total + contrib)
{-# INLINE applyLights #-}


-- | Compute the direct lighting at a particular point for a single light.
applyLight
        :: [Object]     -- possible occluding objects, used for shadows.
        -> Vec3         -- point which is being lit
        -> Vec3         -- surface normal at this point
        -> Light 
        -> Color

applyLight !objs !point !normal !(Light lpoint color)
 = let  -- vector from the light to the surface point
        !dir    = normaliseV3 (lpoint - point)

        -- distance from light source to surface
        !dist   = magnitudeV3 (lpoint - point)

        -- check for occluding objects between the light and the surface point
   in if checkRay objs point dir dist
       then Vec3 0 0 0
       else let -- magnitude of reflection
                !mag    = (normal `dotV3` dir) / (dist * dist)

                -- the light that is reflected
                !refl   = color `mulsV3` mag
            in refl
{-# INLINE applyLight #-}                
               
