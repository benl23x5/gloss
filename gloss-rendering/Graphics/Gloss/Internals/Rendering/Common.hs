{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Rendering.Common 
        ( gf, gsizei
        , withModelview
        , withClearBuffer)
where
import Unsafe.Coerce
import Graphics.Gloss.Internals.Data.Color
import Graphics.Gloss.Internals.Rendering.Color
import Graphics.Rendering.OpenGL               (($=))
import qualified Graphics.Rendering.OpenGL.GL   as GL


-- | The OpenGL library doesn't seem to provide a nice way convert
--	a Float to a GLfloat, even though they're the same thing
--	under the covers.  
--
--  Using realToFrac is too slow, as it doesn't get fused in at
--	least GHC 6.12.1
--
gf :: Float -> GL.GLfloat
gf x = unsafeCoerce x
{-# INLINE gf #-}


-- | Used for similar reasons to above
gsizei :: Int -> GL.GLsizei
gsizei x = unsafeCoerce x
{-# INLINE gsizei #-}


-- | Set up the OpenGL rendering context for orthographic projection and run an
--   action to draw the model.
withModelview
	:: (Int, Int)  -- ^ Width and height of window.
	-> IO ()       -- ^ Action to perform.
	-> IO ()

withModelview (sizeX, sizeY) action
 = do
 	GL.matrixMode	$= GL.Projection
	GL.preservingMatrix
	 $ do
		-- setup the co-ordinate system
	 	GL.loadIdentity
		let (sx, sy)	= (fromIntegral sizeX / 2, fromIntegral sizeY / 2)
		GL.ortho (-sx) sx (-sy) sy 0 (-100)
	
		-- draw the world
		GL.matrixMode 	$= GL.Modelview 0
		action

		GL.matrixMode	$= GL.Projection
	
	GL.matrixMode	$= GL.Modelview 0


-- | Clear the OpenGL buffer with the given background color and run 
--   an action to draw the model.
withClearBuffer 
        :: Color        -- ^ Background color
        -> IO ()        -- ^ Action to perform
        -> IO ()

withClearBuffer clearColor action
 = do   
        -- initialization (done every time in this case)
        -- we don't need the depth buffer for 2d.
        GL.depthFunc    GL.$= Just GL.Always

        -- always clear the buffer to white
        GL.clearColor   GL.$= glColor4OfColor clearColor

        -- on every loop
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

        action

