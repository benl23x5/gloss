{-# LANGUAGE BangPatterns #-}

module Trace where
import Object
import Light
import Vec3


-- Cast a single ray into the scene
traceRay
        :: [Object]     -- objects in scene
        -> [Light]      -- lights in scene
        -> Vec3         -- origin of ray
        -> Vec3         -- direction of ray
        -> Int          -- maximum reflection count
        -> Color        -- visible color for this ray
        
-- too many reflections
traceRay objs lights orig dir 0
        = Vec3 0.0 0.0 0.0

traceRay objs lights orig dir limit
 = case findNearest objs orig dir of

         -- ray didn't intersect any objects
         Nothing                
          -> Vec3 0.0 0.0 0.0

         -- ray hit an object
         Just (obj, pt)
          -> let 
                -- get the surface normal at that point
                !n       = surfaceNormal (objectShape obj) pt

                -- result angle of ray after reflection
                !ndir    = dir - n `mulsV3` (2.0 * (n `dotV3` dir))

                -- see if ray hits anything else
                !refl    = traceRay objs lights pt ndir (limit - 1)
                                
                -- sum the direct lighting of this point
                !lighting 
                        = foldl (+) (Vec3 0.0 0.0 0.0) 
                        $ map (applyLight objs pt n) lights
                        
                -- total incoming light is direct lighting plus reflections
                !color   = objectColor obj pt
                !shine   = objectShine obj pt
        
                !in_light 
                        = refl     `mulsV3` shine
                        + lighting `mulsV3` (1.0 - shine)
                
                -- incoming light is modified by surface color
             in clampV3 (color * in_light) 0.0 1.0
