{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Interface.ViewPort.Motion
	(callback_viewPort_motion)
where
import Graphics.Gloss.ViewPort
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Geometry.Vector
import Graphics.Gloss.Internals.Interface.Callback
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.UI.GLUT						as GLUT
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
	portRef controlRef
	pos
 = do
--	putStr $ "motion pos = " ++ show pos ++ "\n"

	translateMark	<- controlRef `getsIORef` VPC.stateTranslateMark
	rotateMark	<- controlRef `getsIORef` VPC.stateRotateMark

	(case translateMark of
	 Nothing		-> return ()
	 Just (markX, markY)	
	  -> motionTranslate
	  	portRef controlRef 
		(fromIntegral markX, fromIntegral markY) 
		pos )


	(case rotateMark of
	 Nothing		-> return ()
	 Just (markX, markY)
	  -> motionRotate
	  	portRef controlRef
		(fromIntegral markX, fromIntegral markY) 
		pos )


motionTranslate 
	portRef controlRef
	(markX, markY)
	(GL.Position posX posY)
 = do
	(transX, transY)
		<- portRef `getsIORef` viewPortTranslate

	s	<- portRef `getsIORef` viewPortScale
	r	<- portRef `getsIORef` viewPortRotate

	let dX		= fromIntegral $ markX - posX
	let dY		= fromIntegral $ markY - posY

	let offset	= (dX / s, dY / s)

	let (oX, oY)	= rotateV (degToRad r) offset

	portRef `modifyIORef` \s -> s 
		{ viewPortTranslate	
		   = 	( transX - oX
		 	, transY + oY) }
		
	controlRef `modifyIORef` \s -> s
		{ VPC.stateTranslateMark
		   =	Just (fromIntegral posX, fromIntegral posY) }

	GLUT.postRedisplay Nothing


motionRotate 
	portRef controlRef
	(markX, markY)
	(GL.Position posX posY)
 = do
 	rotate		<- portRef    `getsIORef` viewPortRotate
	rotateFactor	<- controlRef `getsIORef` VPC.stateRotateFactor
	
	portRef `modifyIORef` \s -> s 
		{ viewPortRotate
		   = 	rotate + rotateFactor * fromIntegral (posX - markX) }
		
	controlRef `modifyIORef` \s -> s
		{ VPC.stateRotateMark
		   = 	Just (fromIntegral posX, fromIntegral posY) }
	
	GLUT.postRedisplay Nothing


getsIORef ref fun
 = liftM fun $ readIORef ref
