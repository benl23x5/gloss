{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gloss.Internals.Interface.ViewPort.Motion
	(callback_viewPort_motion)
where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Internals.Interface.ViewPort
import Graphics.Gloss.Internals.Interface.Callback
import Graphics.Gloss.Internals.Interface.Backend
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.Rendering.OpenGL.GL					as GL
import Control.Monad	
import Data.IORef


-- | Callback to handle keyboard and mouse button events
--	for controlling the viewport.
callback_viewPort_motion 
	:: IORef ViewPort 	-- ^ ref to ViewPort state
	-> IORef VPC.State 	-- ^ ref to ViewPort Control state
	-> Callback

callback_viewPort_motion portRef controlRef 
 	= Motion (viewPort_motion portRef controlRef)

viewPort_motion
	:: IORef ViewPort
	-> IORef VPC.State
	-> MotionCallback
	
viewPort_motion
	portRef controlRef
	stateRef
	pos
 = do
--	putStr $ "motion pos = " ++ show pos ++ "\n"

	translateMark	<- controlRef `getsIORef` VPC.stateTranslateMark
	rotateMark	<- controlRef `getsIORef` VPC.stateRotateMark

	(case translateMark of
	 Nothing		-> return ()
	 Just (markX, markY)	
	  -> do
		motionTranslate
	  	  portRef controlRef
		  (fromIntegral markX, fromIntegral markY)
		  pos 
		postRedisplay stateRef)


	(case rotateMark of
	 Nothing		-> return ()
	 Just (markX, markY)
	  -> do
		motionRotate
	  	  portRef controlRef
		  (fromIntegral markX, fromIntegral markY)
		  pos 
		postRedisplay stateRef)


motionTranslate
	:: IORef ViewPort
        -> IORef VPC.State
        -> (GL.GLint, GL.GLint)
        -> (Int, Int)
	-> IO ()
 
motionTranslate 
	portRef controlRef
	(markX :: GL.GLint, markY :: GL.GLint)
	(posX, posY)
 = do
	(transX, transY)
		<- portRef `getsIORef` viewPortTranslate

	scale	<- portRef `getsIORef` viewPortScale
	r	<- portRef `getsIORef` viewPortRotate

	let dX		= fromIntegral $ markX - (fromIntegral posX)
	let dY		= fromIntegral $ markY - (fromIntegral posY)

	let offset	= (dX / scale, dY / scale)

	let (oX, oY)	= rotateV (degToRad r) offset

	portRef `modifyIORef` \s -> s 
		{ viewPortTranslate	
		   = 	( transX - oX
		 	, transY + oY) }
		
	controlRef `modifyIORef` \s -> s
		{ VPC.stateTranslateMark
		   =	Just (fromIntegral posX, fromIntegral posY) }


motionRotate
	:: IORef ViewPort
	-> IORef VPC.State
	-> (GL.GLint, GL.GLint)
	-> (Int, Int)
	-> IO ()

motionRotate 
	portRef controlRef
	(markX :: GL.GLint, _markY :: GL.GLint)
	(posX, posY)
 = do
 	rotate		<- portRef    `getsIORef` viewPortRotate
	rotateFactor	<- controlRef `getsIORef` VPC.stateRotateFactor
	
	portRef `modifyIORef` \s -> s 
		{ viewPortRotate
		   = 	rotate + rotateFactor * fromIntegral ((fromIntegral posX) - markX) }
		
	controlRef `modifyIORef` \s -> s
		{ VPC.stateRotateMark
		   = 	Just (fromIntegral posX, fromIntegral posY) }


getsIORef :: IORef a -> (a -> r) -> IO r
getsIORef ref fun
 = liftM fun $ readIORef ref
