{-# OPTIONS_HADDOCK hide #-}

-- | 	The main display function.
module	Graphics.Gloss.Internals.Interface.Window
	( createWindow )
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Color
import Graphics.Gloss.Internals.Interface.Debug
import Graphics.Gloss.Internals.Interface.Callback		(Callback)
import qualified Graphics.Gloss.Internals.Interface.Callback	as Callback

import Graphics.UI.GLUT					(($=), get)
import qualified Graphics.Rendering.OpenGL.GL		as GL
import qualified Graphics.UI.GLUT			as GLUT

import Control.Monad

-- | Open a window and use the supplied callbacks to handle window events.
createWindow	
	:: String 		-- ^ Name of the window.
	-> (Int, Int) 		-- ^ Initial size of the window, in pixels.
	-> (Int, Int)		-- ^ Initial position of the window, in pixels relative to 
				--	the top left corner of the screen.
	-> Color		-- ^ Color to use when clearing.
	-> [Callback]		-- ^ Callbacks to use
	-> IO ()

createWindow
	windowName
	(sizeX, sizeY) 
	(posX,  posY)
	clearColor
	callbacks
 = do
	-- Turn this on to spew debugging info to stdout
	let debug	= False

	-- Initialize GLUT
 	(_progName, _args)	<- GLUT.getArgsAndInitialize
	glutVersion		<- get GLUT.glutVersion

	when debug
	 $ do 	putStr	$ "* displayInWindow\n"
	 	putStr	$ "  glutVersion        = " ++ show glutVersion		++ "\n"

	-- Setup and create a new window.
	--	Be sure to set initialWindow{Position,Size} before calling
	--	createWindow. If we don't do this we get wierd half-created
	--	windows some of the time.
	--
	GLUT.initialWindowPosition	
	 $= 	GL.Position		
	 		(fromIntegral posX)
			(fromIntegral posY)

	GLUT.initialWindowSize	
	 $= 	GL.Size 
			(fromIntegral sizeX) 
			(fromIntegral sizeY)

	GLUT.initialDisplayMode
	 $= 	[ GLUT.RGBMode
		, GLUT.DoubleBuffered]

	-- See if our requested display mode is possible
	displayMode		<- get GLUT.initialDisplayMode
	displayModePossible	<- get GLUT.displayModePossible
	when debug
	 $ do	putStr	$  "  displayMode        = " ++ show displayMode ++ "\n"
	 		++ "       possible      = " ++ show displayModePossible ++ "\n"
			++ "\n"
		
	-- Here we go!
	when debug
	 $ do	putStr	$ "* creating window\n\n"

	_ <- GLUT.createWindow windowName
	GLUT.windowSize	
	 $= 	GL.Size 
			(fromIntegral sizeX)
			(fromIntegral sizeY)
	
	-- Setup callbacks
	GLUT.displayCallback		$= callbackDisplay clearColor callbacks
	GLUT.reshapeCallback		$= Just (callbackReshape  	callbacks)
	GLUT.keyboardMouseCallback	$= Just (callbackKeyMouse 	callbacks)
	GLUT.motionCallback		$= Just (callbackMotion   	callbacks)
	GLUT.idleCallback		$= Just (callbackIdle 		callbacks)

	--  Switch some things.
	--  auto repeat interferes with key up / key down checks.
	--	BUGS: this doesn't seem to work?
	GLUT.perWindowKeyRepeat		$= GLUT.PerWindowKeyRepeatOff	

	-- we don't need the depth buffer for 2d.
	GL.depthFunc	$= Just GL.Always

	-- always clear the buffer to white
	GL.clearColor	$= glColor4OfColor clearColor

	-- Dump some debugging info
	when debug
	 $ do	dumpGlutState
	 	dumpFramebufferState
		dumpFragmentState

	---------------
	-- Call the GLUT mainloop. 
	--	This function will return when something calls GLUT.leaveMainLoop
	--
	--	We can ask for this in freeglut, but it doesn't seem to work :(.
	--	GLUT.actionOnWindowClose	$= GLUT.MainLoopReturns
	when debug
	 $ do	putStr	$ "* entering mainloop..\n"
	
	GLUT.mainLoop

	when debug
	 $	putStr	$ "* all done\n"
	 
	return ()


callbackDisplay :: t -> [Callback] -> IO ()
callbackDisplay _ callbacks
 = do
	-- clear the display
	GL.clear [GL.ColorBuffer, GL.DepthBuffer]
	GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

	-- get the display callbacks from the chain
	let funs	= [f	| (Callback.Display f) <- callbacks]
	sequence_ funs

	-- swap front and back buffers
	GLUT.swapBuffers
	GLUT.reportErrors 
 	return ()


callbackReshape :: [Callback] -> GLUT.Size -> IO ()
callbackReshape callbacks size
 	= sequence_
	$ map 	(\f -> f size)
		[f | Callback.Reshape f 	<- callbacks]


callbackKeyMouse
	:: [Callback]
	-> GLUT.Key
	-> GLUT.KeyState
	-> GLUT.Modifiers
	-> GLUT.Position
	-> IO ()

callbackKeyMouse callbacks key keystate modifiers pos
 	= sequence_ 
	$ map 	(\f -> f key keystate modifiers pos) 
		[f | Callback.KeyMouse f 	<- callbacks]


callbackMotion
	:: [Callback]
	-> GLUT.Position
	-> IO ()

callbackMotion callbacks pos
 	= sequence_
	$ map	(\f -> f pos)
		[f | Callback.Motion f 		<- callbacks]


callbackIdle
	:: [Callback]
	-> IO ()

callbackIdle callbacks
 	= sequence_
	$ [f | Callback.Idle f 			<- callbacks]
	
	
