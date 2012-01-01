{-# LANGUAGE BangPatterns #-}

module Solver where
import World
import Trace
import Vec3
import qualified Graphics.Gloss         as G
import qualified Graphics.Gloss.Field   as G


run :: Int -> Int -> Int -> IO ()                     
run sizeX sizeY zoom 
 = G.animateField 
        (G.InWindow "ray" (sizeX, sizeY) (100, 100)) 
        (zoom, zoom)
        (tracePixel 400 300)


tracePixel :: Int -> Int -> Float -> G.Point -> G.Color
tracePixel !sizeX !sizeY !time (x, y)
 = let  sizeX'  = fromIntegral sizeX
        sizeY'  = fromIntegral sizeY
       
        ambient = Vec3 0.3 0.3 0.3

        Vec3 r g b
                = traceRay 
                        (objs time) 
                        lights 
                        ambient
                        eyePos
                        (normaliseV3 (Vec3 (x * sizeX') ((- y) * sizeY') 0 - eyePos))
                        10

   in   G.rawColor r g b 1.0
{-# INLINE tracePixel #-}

