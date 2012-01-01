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
        
-- too many reflections
traceRay _ _ _ _ _ 0
        = Vec3 0.0 0.0 0.0

traceRay !objs !lights !ambient !orig !dir !limit
 = case castRay objs orig dir of

         -- ray didn't intersect any objects
         Nothing                
          -> Vec3 0.0 0.0 0.0

         -- ray hit an object
         Just (obj, pt)
          -> let 
                -- get the surface normal at that point.
                !n        = surfaceNormal obj pt

                -- result angle of ray after reflection.
                !ndir     = dir - n `mulsV3` (2.0 * (n `dotV3` dir))

                -- see if ray hits anything else.
                !refl     = traceRay objs lights ambient pt ndir (limit - 1)
                                
                -- determine the direct lighting at this point
                !direct   = applyLights objs pt n lights

                -- total lighting is the direct lights plus ambient
                !lighting = direct + ambient
                        
                -- total incoming light is direct lighting plus reflections
                !color   = colorOfObject obj pt
                !shine   = shineOfObject obj pt
        
                !in_light 
                        = refl     `mulsV3` shine
                        + lighting `mulsV3` (1.0 - shine)
                
                -- incoming light is modified by surface color
             in clampV3 (color * in_light) 0.0 1.0
