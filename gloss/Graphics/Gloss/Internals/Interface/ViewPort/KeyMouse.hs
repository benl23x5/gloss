{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PatternGuards #-}

module Graphics.Gloss.Internals.Interface.ViewPort.KeyMouse
	(callback_viewPort_keyMouse)
where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Internals.Interface.ViewPort
import Graphics.Gloss.Internals.Interface.ViewPort.Command
import Graphics.Gloss.Internals.Interface.Backend
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import Control.Monad
import Data.IORef
import Data.Maybe


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
	-> KeyboardMouseCallback

viewPort_keyMouse portRef controlRef stateRef key keyState keyMods pos
 = do	commands	<- controlRef `getsIORef` VPC.stateCommands 

{-	putStr 	$  "keyMouse key      = " ++ show key 		++ "\n"
		++ "keyMouse keyState = " ++ show keyState	++ "\n"
		++ "keyMouse keyMods  = " ++ show keyMods 	++ "\n"
-}
        -- Whether the user is holding down the translate button.
        currentlyTranslating    
                <- liftM (isJust . VPC.stateTranslateMark)
                $ readIORef controlRef

        -- Whether the user is holding down the rotate button.
        currentlyRotating
                <- liftM (isJust . VPC.stateRotateMark)
                $ readIORef controlRef

	viewPort_keyMouse2
	        currentlyTranslating
	        currentlyRotating
	        commands
 where
   viewPort_keyMouse2 currentlyTranslating currentlyRotating commands
	-- restore viewport
	| isCommand commands CRestore key keyMods
	, keyState	== Down
	= do	portRef `modifyIORef` \s -> s 
			{ viewPortScale		= 1
			, viewPortTranslate	= (0, 0) 
			, viewPortRotate	= 0 }
		postRedisplay stateRef

	-- zoom ----------------------------------------
	-- zoom out
	| isCommand commands CBumpZoomOut key keyMods
	, keyState	== Down
	= do	controlZoomOut portRef controlRef
	        postRedisplay stateRef

	-- zoom in
	| isCommand commands CBumpZoomIn key keyMods
	, keyState	== Down
	= do	controlZoomIn portRef controlRef 
	        postRedisplay stateRef
	
	-- bump -------------------------------------
	-- bump left
	| isCommand commands CBumpLeft key keyMods
	, keyState	== Down
	= do	motionBump portRef (20, 0)
	        postRedisplay stateRef

	-- bump right
	| isCommand commands CBumpRight key keyMods
	, keyState	== Down
	= do	motionBump portRef (-20, 0)
	        postRedisplay stateRef

	-- bump up
	| isCommand commands CBumpUp key keyMods
	, keyState	== Down
	= do    motionBump portRef (0, 20)
	        postRedisplay stateRef

	-- bump down
	| isCommand commands CBumpDown key keyMods
	, keyState	== Down
	= do    motionBump portRef (0, -20)
	        postRedisplay stateRef

	-- bump clockwise
	| isCommand commands CBumpClockwise key keyMods
	, keyState	== Down
	= do	portRef `modifyIORef` \s -> s {
			viewPortRotate
				= (\r -> r + 5)
				$ viewPortRotate s }
		postRedisplay stateRef

	-- bump anti-clockwise
	| isCommand commands CBumpCClockwise key keyMods
	, keyState	== Down
	= do	portRef `modifyIORef` \s -> s {
			viewPortRotate
				= (\r -> r - 5)
				$ viewPortRotate s }
		postRedisplay stateRef
		
	-- translation --------------------------------------
	-- start
	| isCommand commands CTranslate key keyMods
	, keyState	== Down
	, not currentlyRotating
	= do	let (posX, posY)	= pos
		controlRef `modifyIORef` \s -> s { 
			VPC.stateTranslateMark 
		 		= Just (  posX
				  	, posY) }
		postRedisplay stateRef

	-- end
	-- We don't want to use 'isCommand' here because the user may have
	-- released the translation modifier key before the mouse button.
	-- and we still want to cancel the translation.
	| currentlyTranslating
	, keyState	== Up
	= do	controlRef `modifyIORef` \s -> s { 
		 	VPC.stateTranslateMark = Nothing }
		postRedisplay stateRef

	-- rotation  ---------------------------------------
	-- start
	| isCommand commands CRotate key keyMods
	, keyState	== Down
	, not currentlyTranslating
	= do	let (posX, posY)	= pos
		controlRef `modifyIORef` \s -> s { 
			VPC.stateRotateMark 
		 		= Just (  posX
				  	, posY) }
		postRedisplay stateRef

	-- end
	-- We don't want to use 'isCommand' here because the user may have
	-- released the rotation modifier key before the mouse button, 
	-- and we still want to cancel the rotation.
	| currentlyRotating
	, keyState	== Up
	= do	controlRef `modifyIORef` \s -> s { 
		 	VPC.stateRotateMark = Nothing }
		postRedisplay stateRef

	-- carry on
	| otherwise
	= return ()


controlZoomIn :: IORef ViewPort -> IORef VPC.State -> IO ()
controlZoomIn portRef controlRef
 = do	scaleStep	<- controlRef `getsIORef` VPC.stateScaleStep
	portRef `modifyIORef` \s -> s { 
	 	viewPortScale = viewPortScale s * scaleStep }


controlZoomOut :: IORef ViewPort -> IORef VPC.State -> IO ()
controlZoomOut portRef controlRef
 = do	scaleStep	<- controlRef `getsIORef` VPC.stateScaleStep 
	portRef `modifyIORef` \s -> s {
	 	viewPortScale = viewPortScale s / scaleStep }


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

 
getsIORef :: IORef a -> (a -> r) -> IO r
getsIORef ref fun
 = liftM fun $ readIORef ref
