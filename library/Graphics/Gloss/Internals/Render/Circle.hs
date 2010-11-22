{-# LANGUAGE BangPatterns, MagicHash, PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Fast(ish) rendering of circles.
module Graphics.Gloss.Internals.Render.Circle  where

import 	Graphics.Gloss.Internals.Render.Common
import	qualified Graphics.Rendering.OpenGL.GL		as GL
import	GHC.Exts

-- | Render a circle with the given thickness
renderCircle :: Float -> Float -> Float -> Float -> Float -> IO ()
renderCircle posX posY scaleFactor radius thickness
 	| radScreen	<- scaleFactor * radius
	, steps		<- circleSteps (2 * radScreen)
	= if thickness == 0 
		then renderCircleLine posX posY steps radius
		else renderCircleStrip posX posY steps radius thickness


-- | Decide how many line segments to use to render the circle
circleSteps :: Float -> Int
circleSteps sDiam
	| sDiam < 1	= 1
	| sDiam < 2	= 4
	| sDiam < 10	= 8
	| sDiam < 20	= 16
	| sDiam < 30	= 32
	| otherwise	= 40


renderCircleLine_step :: Float# -> Float# -> Float# -> Float# -> Float# -> Float# -> IO ()
renderCircleLine_step posX posY tStep tStop rad tt
	| tt `geFloat#` tStop
	= return ()
	
	| otherwise
	= do	GL.vertex 
		 $ GL.Vertex2 
			(gf $ F# (posX `plusFloat#` (rad `timesFloat#` (cosFloat# tt))))
			(gf $ F# (posY `plusFloat#` (rad `timesFloat#` (sinFloat# tt))))

		renderCircleLine_step posX posY tStep tStop rad (tt `plusFloat#` tStep)

-- | Render a circle as a line.
renderCircleLine :: Float -> Float -> Int -> Float -> IO ()
renderCircleLine (F# posX) (F# posY) steps (F# rad)
 = let 	n		= fromIntegral steps
	!(F# tStep)	= (2 * pi) / n
	!(F# tStop)	= (2 * pi)

   in	GL.renderPrimitive GL.LineLoop
   		$ renderCircleLine_step posX posY tStep tStop rad 0.0#


-- | Render a circle with a given thickness as a triangle strip
renderCircleStrip :: Float -> Float -> Int -> Float -> Float -> IO ()
renderCircleStrip (F# posX) (F# posY) steps r width
 = let	n		= fromIntegral steps
	!(F# tStep)	= (2 * pi) / n
	!(F# tStop)	= (2 * pi) + (F# tStep) / 2
	!(F# r1)	= r - width / 2
	!(F# r2)	= r + width / 2

   in	GL.renderPrimitive GL.TriangleStrip
   		$ renderCircleStrip_step posX posY tStep tStop r1 0.0# r2 
			(tStep `divideFloat#` 2.0#)
   
renderCircleStrip_step 
	:: Float# -> Float# 
	-> Float# -> Float# 
	-> Float# -> Float# -> Float# -> Float# -> IO ()

renderCircleStrip_step posX posY tStep tStop r1 t1 r2 t2
	| t1 `geFloat#` tStop
	= return ()
	
	| otherwise
	= do	GL.vertex 
	 	 $ GL.Vertex2 
			(gf $ F# (posX `plusFloat#` (r1 `timesFloat#` (cosFloat# t1)))) 
			(gf $ F# (posY `plusFloat#` (r1 `timesFloat#` (sinFloat# t1))))

		GL.vertex 
		 $ GL.Vertex2 
			(gf $ F# (posX `plusFloat#` (r2 `timesFloat#` (cosFloat# t2))))
			(gf $ F# (posY `plusFloat#` (r2 `timesFloat#` (sinFloat# t2))))
		
		renderCircleStrip_step posX posY tStep tStop r1 
			(t1 `plusFloat#` tStep) r2 (t2 `plusFloat#` tStep)
	
