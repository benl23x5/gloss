{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PatternGuards #-}

module Graphics.Gloss.Internals.Interface.ViewPort.KeyMouse
	(callback_viewPort_keyMouse)
where
import Graphics.Gloss.Interface.ViewPort
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Internals.Interface.ViewPort.Command
import Graphics.Gloss.Internals.Interface.Callback
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.UI.GLUT						as GLUT
import qualified Graphics.Rendering.OpenGL.GL					as GL
import Data.IORef
import Control.Monad


-- | Callback to handle keyboard and mouse button events
--	for controlling the viewport.
callback_viewPort_keyMouse 
	:: IORef ViewPort 	-- ^ ref to ViewPort state
	-> IORef VPC.State 	-- ^ ref to ViewPort Control state
	-> Callback

callback_viewPort_keyMouse portRef controlRef 
 	= KeyMouse (viewPort_keyMouse portRef controlRef)


viewPort_keyMouse
	:: IORef ViewPort
	-> IORef VPC.State
	-> GLUT.Key
	-> GLUT.KeyState
	-> GLUT.Modifiers
	-> GL.Position
	-> IO ()

viewPort_keyMouse portRef controlRef key keyState keyMods pos
 = do	commands	<- controlRef `getsIORef` VPC.stateCommands 

{-	putStr 	$  "keyMouse key      = " ++ show key 		++ "\n"
		++ "keyMouse keyState = " ++ show keyState	++ "\n"
		++ "keyMouse keyMods  = " ++ show keyMods 	++ "\n"
-}
	viewPort_keyMouse2 commands
 where
   viewPort_keyMouse2 commands
	-- restore viewport
	| isCommand commands CRestore key keyMods
	, keyState	== GLUT.Down
	= do	portRef `modifyIORef` \s -> s 
			{ viewPortScale		= 1
			, viewPortTranslate	= (0, 0) 
			, viewPortRotate	= 0 }
		GLUT.postRedisplay Nothing

	-- zoom ----------------------------------------
	-- zoom out
	| isCommand commands CBumpZoomOut key keyMods
	, keyState	== GLUT.Down
	= 	controlZoomOut portRef controlRef

	-- zoom in
	| isCommand commands CBumpZoomIn key keyMods
	, keyState	== GLUT.Down
	= 	controlZoomIn portRef controlRef
	
	-- bump -------------------------------------
	-- bump left
	| isCommand commands CBumpLeft key keyMods
	, keyState	== GLUT.Down
	= 	motionBump portRef (20, 0)

	-- bump right
	| isCommand commands CBumpRight key keyMods
	, keyState	== GLUT.Down
	= 	motionBump portRef (-20, 0)

	-- bump up
	| isCommand commands CBumpUp key keyMods
	, keyState	== GLUT.Down
	= 	motionBump portRef (0, 20)

	-- bump down
	| isCommand commands CBumpDown key keyMods
	, keyState	== GLUT.Down
	= 	motionBump portRef (0, -20)

	-- bump clockwise
	| isCommand commands CBumpClockwise key keyMods
	, keyState	== GLUT.Down
	= do	portRef `modifyIORef` \s -> s {
			viewPortRotate
				= (\r -> r + 5)
				$ viewPortRotate s }
		GLUT.postRedisplay Nothing	

	-- bump anti-clockwise
	| isCommand commands CBumpCClockwise key keyMods
	, keyState	== GLUT.Down
	= do	portRef `modifyIORef` \s -> s {
			viewPortRotate
				= (\r -> r - 5)
				$ viewPortRotate s }
		GLUT.postRedisplay Nothing
		
	-- translation --------------------------------------
	-- start
	| isCommand commands CTranslate key keyMods
	, keyState	== GLUT.Down
	= do	let GL.Position posX posY	= pos
		controlRef `modifyIORef` \s -> s { 
			VPC.stateTranslateMark 
		 		= Just (  fromIntegral posX
				  	, fromIntegral posY) }
		GLUT.postRedisplay Nothing

	-- end
	| isCommand commands CTranslate key keyMods
	, keyState	== GLUT.Up
	= do	controlRef `modifyIORef` \s -> s { 
		 	VPC.stateTranslateMark = Nothing }
		GLUT.postRedisplay Nothing

	-- rotation  ---------------------------------------
	-- start
	| isCommand commands CRotate key keyMods
	, keyState	== GLUT.Down
	= do	let GL.Position posX posY	= pos
		controlRef `modifyIORef` \s -> s { 
			VPC.stateRotateMark 
		 		= Just (  fromIntegral posX
				  	, fromIntegral posY) }
		GLUT.postRedisplay Nothing

	-- end
	| isCommand commands CRotate key keyMods
	, keyState	== GLUT.Up
	= do	controlRef `modifyIORef` \s -> s { 
		 	VPC.stateRotateMark = Nothing }
		GLUT.postRedisplay Nothing

	-- carry on
	| otherwise
	= return ()


controlZoomIn :: IORef ViewPort -> IORef VPC.State -> IO ()
controlZoomIn portRef controlRef
 = do	scaleStep	<- controlRef `getsIORef` VPC.stateScaleStep
	portRef `modifyIORef` \s -> s { 
	 	viewPortScale = viewPortScale s * scaleStep }
	GLUT.postRedisplay Nothing


controlZoomOut :: IORef ViewPort -> IORef VPC.State -> IO ()
controlZoomOut portRef controlRef
 = do	scaleStep	<- controlRef `getsIORef` VPC.stateScaleStep 
	portRef `modifyIORef` \s -> s {
	 	viewPortScale = viewPortScale s / scaleStep }
	GLUT.postRedisplay Nothing


motionBump :: IORef ViewPort -> (Float, Float) -> IO ()
motionBump
	portRef
	(bumpX, bumpY)
 = do
	(transX, transY)
		<- portRef `getsIORef` viewPortTranslate

	scale	<- portRef `getsIORef` viewPortScale
	r	<- portRef `getsIORef` viewPortRotate

	let offset	= (bumpX / scale, bumpY / scale)

	let (oX, oY)	= rotateV (degToRad r) offset

	portRef `modifyIORef` \s -> s 
		{ viewPortTranslate	
		   = 	( transX - oX
		 	, transY + oY) }
			
	GLUT.postRedisplay Nothing

 
getsIORef :: IORef a -> (a -> r) -> IO r
getsIORef ref fun
 = liftM fun $ readIORef ref
