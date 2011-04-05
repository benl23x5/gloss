{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}

module Graphics.Gloss.Internals.Render.Picture
	( renderPicture )
where
import	Graphics.Gloss.Data.Picture
import	Graphics.Gloss.Data.Color
import	Graphics.Gloss.Internals.Interface.ViewPort
import	Graphics.Gloss.Internals.Render.Options
import	Graphics.Gloss.Internals.Render.Common
import	Graphics.Gloss.Internals.Render.Circle
import	Graphics.Gloss.Internals.Render.Bitmap
import	Graphics.UI.GLUT			(($=), get)
import	qualified Graphics.Rendering.OpenGL.GL	as GL
import	qualified Graphics.UI.GLUT		as GLUT
import   Control.Monad

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
	(matProj_  :: GL.GLmatrix GL.GLdouble)	
			<- get $ GL.matrix (Just $ GL.Projection)
	viewport_  	<- get $ GL.viewport
	windowSize_	<- get GLUT.windowSize

	-- 
	let ?modeWireframe	= optionsWireframe renderS
	    ?modeColor		= optionsColor     renderS
	    ?matProj		= matProj_
	    ?viewport		= viewport_
	    ?windowSize		= windowSize_
	
	-- setup render state for world
	setLineSmooth	(optionsLineSmooth renderS)
	setBlendAlpha	(optionsBlendAlpha renderS)
	
	drawPicture (viewPortScale viewS) picture

drawPicture
	:: ( ?modeWireframe :: Bool
	   , ?modeColor :: Bool) 
	=> Float -> Picture -> IO ()	  

drawPicture circScale picture
 = {-# SCC "drawComponent" #-}
   case picture of

	-- nothin'
	Blank
	 -> 	return ()

	-- line
 	Line path	
	 -> GL.renderPrimitive GL.LineStrip 
		$ vertexPFs path


	-- polygon (where?)
	Polygon path
	 | ?modeWireframe
	 -> GL.renderPrimitive GL.LineLoop
	 	$ vertexPFs path
		
	 | otherwise
	 -> GL.renderPrimitive GL.Polygon
	 	$ vertexPFs path

	-- circle
	Circle radius
	 ->  renderCircle 0 0 circScale radius 0
	
	ThickCircle radius thickness
	 ->  renderCircle 0 0 circScale radius thickness
	
	-- stroke text
	-- 	text looks wierd when we've got blend on,
	--	so disable it during the renderString call.
	Text str 
	 -> do
	 	GL.blend	$= GL.Disabled
		GLUT.renderString GLUT.Roman str
		GL.blend	$= GL.Enabled

	-- colors with float components.
	Color col p
	 |  ?modeColor
	 ->  {-# SCC "draw.color" #-}
   	     do	oldColor 	 <- get GL.currentColor

		let (r, g, b, a) = rgbaOfColor col

		GL.currentColor	 $= GL.Color4 (gf r) (gf g) (gf b) (gf a)
		drawPicture circScale p
		GL.currentColor	$= oldColor		

	 |  otherwise
	 -> 	drawPicture circScale p


	-- ease up on GL.preservingMatrix
	--	This is an important optimisation for the Eden example,
	--	as it draws lots of translated circles.
	Translate posX posY (Circle radius)
	 -> renderCircle posX posY circScale radius 0

	Translate posX posY (ThickCircle radius thickness)
	 -> renderCircle posX posY circScale radius thickness

	Translate tx ty (Rotate deg p)
	 -> GL.preservingMatrix
	  $ do	GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
		GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
		drawPicture circScale p

	-----
	Translate tx ty	p
	 -> GL.preservingMatrix
	  $ do	GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
		drawPicture circScale p

	Rotate deg p
	 -> GL.preservingMatrix
	  $ do	GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
		drawPicture circScale p

	Scale sx sy p
	 -> GL.preservingMatrix
	  $ do	GL.scale (gf sx) (gf sy) 1
		let mscale	= max sx sy
		drawPicture (circScale * mscale) p
			
	-----
	Bitmap width height imgData
	 -> do	-- As OpenGL reads texture pixels as ABGR (instead of RGBA)
		--  each pixel's value needs to be reversed we also need to
		--  Convert imgData from ByteString to Ptr Word8
		imgData' <- reverseRGBA $ imgData

		-- Allocate texture handle for texture
		[texObject] <- GL.genObjectNames 1
		GL.textureBinding GL.Texture2D $= Just texObject

		-- Sets the texture in imgData as the current texture
		GL.texImage2D
			Nothing
			GL.NoProxy
			0
			GL.RGBA8
			(GL.TextureSize2D
				(gsizei width)
				(gsizei height))
			0
			(GL.PixelData GL.RGBA GL.UnsignedInt8888 imgData')
		-- Set up wrap and filtering mode
		GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
		GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
		GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)
		
		-- Enable texturing
		GL.texture GL.Texture2D $= GL.Enabled
		GL.textureFunction      $= GL.Combine
		
		-- Set current texture
		GL.textureBinding GL.Texture2D $= Just texObject
		
		-- Set to opaque
		GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
		
		-- Draw textured polygon
		GL.renderPrimitive GL.Polygon
		 $ do zipWithM_
		        (\(pX, pY) (tX, tY)
			  -> do GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
		           	GL.vertex   $ GL.Vertex2   (gf pX) (gf pY))

			(bitmapPath (fromIntegral width) (fromIntegral height))
			        [(0,0), (1.0,0), (1.0,1.0), (0,1.0)]

		-- Disable texturing
		GL.texture GL.Texture2D $= GL.Disabled

	Pictures ps
	 -> mapM_ (drawPicture circScale) ps
	

-- Utils ------------------------------------------------------------------------------------------
-- | Turn alpha blending on or off
setBlendAlpha :: Bool -> IO ()
setBlendAlpha state
 	| state	
 	= do	GL.blend	$= GL.Enabled
		GL.blendFunc	$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

	| otherwise
 	= do	GL.blend	$= GL.Disabled
		GL.blendFunc	$= (GL.One, GL.Zero) 	

-- | Turn line smoothing on or off
setLineSmooth :: Bool -> IO ()
setLineSmooth state
	| state		= GL.lineSmooth	$= GL.Enabled
	| otherwise	= GL.lineSmooth $= GL.Disabled


vertexPFs ::	[(Float, Float)] -> IO ()
{-# INLINE vertexPFs #-}
vertexPFs []	= return ()
vertexPFs ((x, y) : rest)
 = do	GL.vertex $ GL.Vertex2 (gf x) (gf y)
 	vertexPFs rest



