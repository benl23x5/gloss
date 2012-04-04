{-# LANGUAGE BangPatterns #-}

module Trace where
import Object
import Light
import Vec3


-- Cast a single ray into the scene
traceRay
        :: [Object]     -- objects in scene
        -> [Light]      -- direct lights in scene
        -> Color        -- ambient light in scene
        -> Vec3         -- origin of ray
        -> Vec3         -- direction of ray
        -> Int          -- maximum reflection count
        -> Color        -- visible color for this ray
        
traceRay !objs !lights !ambient !(Vec3 gX gY gZ) !dir !limit
 = go gX gY gZ dir limit
 where 
       -- too many reflections,
       -- give up incase we've found two parallel mirrors..
       go _ _ _ _ 0
        = Vec3 0.0 0.0 0.0

       go !oX !oY oZ !dir' !bounces
        = castRay_continuation objs (Vec3 oX oY oZ) dir' 
            -- ray didn't intersect any objects
            (Vec3 0.0 0.0 0.0)

            -- ray hit an object
            (\obj point@(Vec3 pX' pY' pZ')
             -> let 
                -- get the surface normal at that point.
                !normal    = surfaceNormal obj point

                -- result angle of ray after reflection.
                !newdir    = dir - normal `mulsV3` (2.0 * (normal `dotV3` dir))
 
                -- determine the direct lighting at this point
                !direct    = applyLights objs point normal lights

                -- see if ray hits anything else.
                !refl      = go pX' pY' pZ' newdir (bounces - 1)

                -- total lighting is the direct lights plus ambient
                !lighting  = direct + ambient
                        
                -- total incoming light is direct lighting plus reflections
                !color     = colorOfObject obj point
                !shine     = shineOfObject obj point
        
                !light_in  = refl    `mulsV3` shine 
                           + lighting `mulsV3` (1.0 - shine)
                
                -- Outgoing light is incoming light modified by surface color.
                -- We also need to clip it incase the sum of all incoming lights
                --  will be too bright to display.
                !light_out = clipV3 (light_in * color) 1.0

              in light_out)
{-# INLINE traceRay #-}
