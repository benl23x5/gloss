{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}

module Graphics.Gloss.Render.Picture
	( renderPicture )
where

import	Graphics.Gloss.Render.Options
import	Graphics.Gloss.ViewPort
import	Graphics.Gloss.Picture
import	Graphics.Gloss.Color

import	Graphics.UI.GLUT			(($=), get)
import  qualified Graphics.Rendering.OpenGL.GLU.Matrix as GLU
import	qualified Graphics.Rendering.OpenGL.GL	as GL
import	qualified Graphics.UI.GLUT		as GLUT

import 	Data.IORef
import	Control.Monad


-- ^ Render a picture using the given render options and viewport.
renderPicture
	:: Options 		-- ^ The render options to use
	-> ViewPort		-- ^ The current viewport.
	-> Picture 		-- ^ The picture to render.
	-> IO ()

renderPicture
	renderS
	viewS
	picture
 = do
	-- This GL state doesn't change during rendering, 
	--	so we can just read it once here
	(matProj_  :: GL.GLmatrix GL.GLdouble)	<- get $ GL.matrix (Just $ GL.Projection)
	viewport_  				<- get $ GL.viewport
	windowSize_				<- get GLUT.windowSize

	-- 
	let ?modeWireframe	= optionsWireframe renderS
	    ?modeColor		= optionsColor     renderS
	    ?scale		= viewPortScale    viewS
	    ?matProj		= matProj_
	    ?viewport		= viewport_
	    ?windowSize		= windowSize_
	
	-- setup render state for world
	setLineSmooth	(optionsLineSmooth renderS)
	setBlendAlpha	(optionsBlendAlpha renderS)
	
	drawPicture picture
	  
drawPicture picture
 = {-# SCC "drawComponent" #-}
   case picture of

	-- nothin'
	Blank
	 -> 	return ()

	-- line
 	Line path	
	 -> {-# SCC "draw.line" #-} 
	    GL.renderPrimitive GL.LineStrip 
		$ mapM_ vertexPF path


	-- polygon (where?)
	Polygon path
	 | ?modeWireframe
	 -> GL.renderPrimitive GL.LineLoop
	 	$ mapM_ vertexPF path
		
	 | otherwise
	 -> GL.renderPrimitive GL.Polygon
	 	$ mapM_ vertexPF path

	-- circle
	Circle diam width
	 ->  {-# SCC "draw.circle" #-} 
	     --renderCircle 0 0 ?scale diam width
	     error "circle"
	
	-- stroke text
	-- 	text looks wierd when we've got blend on,
	--	so disable it during the renderString call.
	Text str 
	 -> do
	 	GL.blend	$= GL.Disabled
		GLUT.renderString GLUT.Roman str
		GL.blend	$= GL.Enabled

	-- colors with float components.
	Color (RGBA r g b a) p
	 |  ?modeColor
	 ->  {-# SCC "draw.color" #-}
   	     do
		oldColor 	<- get GL.currentColor
		
		GL.currentColor	
			$= GL.Color4 (gf r) (gf g) (gf b) (gf a)
		drawPicture p

		GL.currentColor	$= oldColor		

	 |  otherwise
	 -> 	drawPicture p

	-- colors with 8 bit integer components.
	Color (RGBA8 r g b a) p
	 | ?modeColor
	 -> do
		let rF	= fromIntegral r / 255
		let gF	= fromIntegral g / 255
		let bF	= fromIntegral b / 255
		let aF	= fromIntegral a / 255

		GL.color $ GL.Color4 rF gF bF (aF :: GL.GLfloat)
		drawPicture p

	 | otherwise
	 ->	drawPicture p

	-- transform
{-	Translate posX posY (Circle radius width)
	 -> {-# SCC "draw.transCircle" #-} 
	    renderCircle posX posY ?scale radius width
-}
	Translate tx ty	p
	 -> GL.preservingMatrix
	     $ do	GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
			drawPicture p

	Rotate deg p
	 -> GL.preservingMatrix
	     $ do	GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
			drawPicture p

	Scale sx sy p
	 -> GL.preservingMatrix
	     $ do	GL.scale (gf sx) (gf sy) 1
			drawPicture p
			
	Pictures ps
	 -> mapM_ drawPicture ps
	

-- Utils ------------------------------------------------------------------------------------------
-- | Turn alpha blending on or off
setBlendAlpha state
 	| state	
 	= do	GL.blend	$= GL.Enabled
		GL.blendFunc	$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

	| otherwise
 	= do	GL.blend	$= GL.Disabled
		GL.blendFunc	$= (GL.One, GL.Zero) 	

-- | Turn line smoothing on or off
setLineSmooth state
	| state		= GL.lineSmooth	$= GL.Enabled
	| otherwise	= GL.lineSmooth $= GL.Disabled


vertexFF :: 	Float -> Float -> IO ()
vertexFF 	x	y	
	= GL.vertex $ GL.Vertex2 (gf x) (gf y) 


vertexPF ::	(Float, Float) -> IO ()
vertexPF	(x, y)
	= GL.vertex $ GL.Vertex2 (gf x) (gf y)

gf :: Float -> GL.GLfloat
gf = realToFrac

