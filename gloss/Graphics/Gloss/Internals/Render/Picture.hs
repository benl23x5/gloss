{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Graphics.Gloss.Internals.Render.Picture
	(renderPicture)
where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Render.State
import Graphics.Gloss.Internals.Render.Common
import Graphics.Gloss.Internals.Render.Circle
import Graphics.Gloss.Internals.Render.Bitmap
import System.Mem.StableName
import Foreign.ForeignPtr
import Data.IORef
import Data.List
import Control.Monad
import Graphics.Rendering.OpenGL	               	(($=), get)
import qualified Graphics.Rendering.OpenGL.GL	        as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors   as GLU
import qualified Graphics.UI.GLUT		        as GLUT


-- | Render a picture using the given render options and viewport.
renderPicture
	:: forall a . Backend a
	=> IORef a
	-> State		-- ^ The render state
	-> ViewPort		-- ^ The current viewport.
	-> Picture 		-- ^ The picture to render.
	-> IO ()

renderPicture
	backendRef
	renderS
	viewS
	picture
 = do
	-- This GL state doesn't change during rendering, 
	--	so we can just read it once here
	(matProj_  :: GL.GLmatrix GL.GLdouble)	
			<- get $ GL.matrix (Just GL.Projection)
	viewport_  	<- get $ GL.viewport
	windowSize_	<- getWindowDimensions backendRef

	-- 
	let ?modeWireframe	= stateWireframe renderS
	    ?modeColor		= stateColor     renderS
	    ?refTextures        = stateTextures  renderS
	    ?matProj		= matProj_
	    ?viewport		= viewport_
	    ?windowSize		= windowSize_
	
	-- setup render state for world
	setLineSmooth	(stateLineSmooth renderS)
	setBlendAlpha	(stateBlendAlpha renderS)
	
	-- Adjust the picture
	let picture'		= applyViewPortToPicture viewS picture
        checkErrors "before drawPicture."
        drawPicture (viewPortScale viewS) picture'
        checkErrors "after drawPicture."


drawPicture
	:: ( ?modeWireframe     :: Bool
	   , ?modeColor         :: Bool
	   , ?refTextures       :: IORef [Texture])
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
	
        -- arc
        Arc a1 a2 radius
         ->  renderArc 0 0 circScale radius a1 a2 0
             
        ThickArc a1 a2 radius thickness
         ->  renderArc 0 0 circScale radius a1 a2 thickness
             
	-- stroke text
	-- 	text looks weird when we've got blend on,
	--	so disable it during the renderString call.
	Text str 
	 -> do
	 	GL.blend	$= GL.Disabled
                GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
		GL.blend	$= GL.Enabled

	-- colors with float components.
	Color col p
	 |  ?modeColor
	 ->  do	oldColor 	 <- get GL.currentColor

		let (r, g, b, a) = rgbaOfColor col

		GL.currentColor	 $= GL.Color4 (gf r) (gf g) (gf b) (gf a)
		drawPicture circScale p
		GL.currentColor	$= oldColor		

	 |  otherwise
	 -> 	drawPicture circScale p


        -- Translation --------------------------
        -- Easy translations are done directly to avoid calling GL.perserveMatrix.
	Translate posX posY (Circle radius)
	 -> renderCircle posX posY circScale radius 0

	Translate posX posY (ThickCircle radius thickness)
	 -> renderCircle posX posY circScale radius thickness

	Translate posX posY (Arc a1 a2 radius)
	 -> renderArc posX posY circScale radius a1 a2 0

	Translate posX posY (ThickArc a1 a2 radius thickness)
	 -> renderArc posX posY circScale radius a1 a2 thickness
             
	Translate tx ty (Rotate deg p)
	 -> GL.preservingMatrix
	  $ do	GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
		GL.rotate    (gf deg) (GL.Vector3 0 0 (-1))
		drawPicture circScale p

	Translate tx ty	p
	 -> GL.preservingMatrix
	  $ do	GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
		drawPicture circScale p


        -- Rotation -----------------------------
        -- Easy rotations are done directly to avoid calling GL.perserveMatrix.
        Rotate _   (Circle radius)
         -> renderCircle   0 0 circScale radius 0

        Rotate _   (ThickCircle radius thickness)
         -> renderCircle   0 0 circScale radius thickness

        Rotate deg (Arc a1 a2 radius)
         -> renderArc      0 0 circScale radius (a1-deg) (a2-deg) 0

        Rotate deg (ThickArc a1 a2 radius thickness)
         -> renderArc      0 0 circScale radius (a1-deg) (a2-deg) thickness

        
	Rotate deg p
	 -> GL.preservingMatrix
	  $ do	GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
		drawPicture circScale p


        -- Scale --------------------------------
	Scale sx sy p
	 -> GL.preservingMatrix
	  $ do	GL.scale (gf sx) (gf sy) 1
		let mscale	= max sx sy
		drawPicture (circScale * mscale) p
			
	-- Bitmap -------------------------------
	Bitmap width height imgData cacheMe
	 -> do	
                -- Load the image data into a texture,
                -- or grab it from the cache if we've already done that before.
	        tex     <- loadTexture ?refTextures width height imgData cacheMe
	 
		-- Set up wrap and filtering mode
		GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
		GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
		GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)
		
		-- Enable texturing
		GL.texture GL.Texture2D $= GL.Enabled
		GL.textureFunction      $= GL.Combine
		
		-- Set current texture
		GL.textureBinding GL.Texture2D $= Just (texObject tex)
		
		-- Set to opaque
		GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
		
		-- Draw textured polygon
		GL.renderPrimitive GL.Polygon
		 $ zipWithM_
		        (\(pX, pY) (tX, tY)
			  -> do GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
		           	GL.vertex   $ GL.Vertex2   (gf pX) (gf pY))

			(bitmapPath (fromIntegral width) (fromIntegral height))
			        [(0,0), (1.0,0), (1.0,1.0), (0,1.0)]

		-- Disable texturing
		GL.texture GL.Texture2D $= GL.Disabled

                -- Free uncachable texture objects.
                freeTexture tex
                

	Pictures ps
	 -> mapM_ (drawPicture circScale) ps
	
-- Errors ---------------------------------------------------------------------
checkErrors :: String -> IO ()
checkErrors place
 = do   errors          <- get $ GLU.errors
        when (not $ null errors)
         $ mapM_ (handleError place) errors

handleError :: String -> GLU.Error -> IO ()
handleError place err
 = case err of
    GLU.Error GLU.StackOverflow _
     -> error $ unlines 
      [ "Gloss / OpenGL Stack Overflow " ++ show place
      , "  This program uses the Gloss vector graphics library, which tried to"
      , "  draw a picture using more nested transforms (Translate/Rotate/Scale)"
      , "  than your OpenGL implementation supports. The OpenGL spec requires"
      , "  all implementations to have a transform stack depth of at least 32,"
      , "  and Gloss tries not to push the stack when it doesn't have to, but"
      , "  that still wasn't enough."
      , ""
      , "  You should complain to your harware vendor that they don't provide"
      , "  a better way to handle this situation at the OpenGL API level."
      , ""
      , "  To make this program work you'll need to reduce the number of nested"
      , "  transforms used when defining the Picture given to Gloss. Sorry." ]

    -- Issue #32: Spurious "Invalid Operation" errors under Windows 7 64-bit.
    --   When using GLUT under Windows 7 it complains about InvalidOperation, 
    --   but doesn't provide any other details. All the examples look ok, so 
    --   we're just ignoring the error for now.
    GLU.Error GLU.InvalidOperation _
     -> return ()
    _ 
     -> error $ unlines 
     [  "Gloss / OpenGL Internal Error " ++ show place
     ,  "  Please report this on haskell-gloss@googlegroups.com."
     ,  show err ]


-- Textures -------------------------------------------------------------------
-- | Load a texture.
--   If we've seen it before then use the pre-installed one from the texture
--   cache, otherwise load it into OpenGL.
loadTexture
        :: IORef [Texture]
        -> Int -> Int -> BitmapData
        -> Bool
        -> IO Texture

loadTexture refTextures width height imgData cacheMe
 = do   textures        <- readIORef refTextures

        -- Try and find this same texture in the cache.
        name            <- makeStableName imgData
        let mTexCached      
                = find (\tex -> texName   tex == name
                             && texWidth  tex == width
                             && texHeight tex == height)
                textures
                
        case mTexCached of
         Just tex
          ->    return tex
                
         Nothing
          -> do tex     <- installTexture width height imgData cacheMe
                when cacheMe
                 $ writeIORef refTextures (tex : textures)
                return tex


-- | Install a texture into OpenGL.
installTexture     
        :: Int -> Int
        -> BitmapData
        -> Bool
        -> IO Texture

installTexture width height bitmapData@(BitmapData _ fptr) cacheMe
 = do   
	-- Allocate texture handle for texture
	[tex] <- GL.genObjectNames 1
	GL.textureBinding GL.Texture2D $= Just tex

	-- Sets the texture in imgData as the current texture
	-- This copies the data from the pointer into OpenGL texture memory, 
	-- so it's ok if the foreignptr gets garbage collected after this.
        withForeignPtr fptr
         $ \ptr ->
   	   GL.texImage2D
#if MIN_VERSION_OpenGL(2,9,0)
		GLUT.Texture2D
#else
		Nothing
#endif
		GL.NoProxy
		0
		GL.RGBA8
		(GL.TextureSize2D
			(gsizei width)
			(gsizei height))
		0
		(GL.PixelData GL.RGBA GL.UnsignedInt8888 ptr)

        -- Make a stable name that we can use to identify this data again.
        -- If the user gives us the same texture data at the same size then we
        -- can avoid loading it into texture memory again.
        name    <- makeStableName bitmapData

        return  Texture
                { texName       = name
                , texWidth      = width
                , texHeight     = height
                , texData       = fptr
                , texObject     = tex
                , texCacheMe    = cacheMe }


-- | If this texture does not have its `cacheMe` flag set then delete it from 
--   OpenGL and free the memory.
freeTexture :: Texture -> IO ()
freeTexture tex
 | texCacheMe tex       = return ()
 | otherwise            = GL.deleteObjectNames [texObject tex]



-- Utils ----------------------------------------------------------------------
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



