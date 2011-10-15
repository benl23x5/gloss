{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}

-- | Handling the current viewport during rendering.
module Graphics.Gloss.Internals.Render.ViewPort
	( withViewPort )
where
import	Graphics.Gloss.Internals.Interface.Backend
import	Graphics.Gloss.Internals.Interface.ViewPort
import	Graphics.Rendering.OpenGL					(GLfloat, ($=))
import	qualified Graphics.Rendering.OpenGL.GL				as GL

import	Data.IORef							(IORef)

-- | Perform a rendering action whilst using the given viewport
withViewPort
	:: forall a . Backend a
	=> IORef a
	-> ViewPort 		-- ^ The viewport to use.
	-> IO () 		-- ^ The rendering action to perform.
	-> IO ()

withViewPort backendRef port action
 = do
 	GL.matrixMode	$= GL.Projection
	GL.preservingMatrix
	 $ do
		-- setup the co-ordinate system
	 	GL.loadIdentity
		(sizeX, sizeY) 	<- getWindowDimensions backendRef
		let (sx, sy)	= (fromIntegral sizeX / 2, fromIntegral sizeY / 2)

		GL.ortho (-sx) sx (-sy) sy 0 (-100)
	
		-- draw the world
		GL.matrixMode 	$= GL.Modelview 0
		GL.preservingMatrix
		 $ do
			GL.loadIdentity
			let rotate :: GLfloat	= realToFrac $ viewPortRotate port
			let transX :: GLfloat	= realToFrac $ fst $ viewPortTranslate port
			let transY :: GLfloat   = realToFrac $ snd $ viewPortTranslate port
		 	let scale  :: GLfloat	= realToFrac $ viewPortScale port

			-- apply the global view transforms
			GL.scale     scale  scale  1
			GL.rotate    rotate (GL.Vector3 0 0 1)
			GL.translate (GL.Vector3 transX transY 0)

			-- call the client render action
			action 

		GL.matrixMode	$= GL.Projection
	
	GL.matrixMode	$= GL.Modelview 0
