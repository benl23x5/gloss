{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PatternGuards #-}

-- | Callback for exiting the program.
module Graphics.Gloss.Internals.Interface.Common.Exit
	(callback_exit)
where
import Graphics.Gloss.Internals.Interface.Callback
import qualified Graphics.UI.GLUT		as GLUT
import qualified System.Exit			as System

callback_exit :: a -> Callback
callback_exit stateRef
 =	KeyMouse (keyMouse_exit stateRef)

keyMouse_exit :: a -> GLUT.KeyboardMouseCallback
keyMouse_exit
	_
	key keyState _
	_

	-- exit
	| key		== GLUT.Char '\27'
	, keyState	== GLUT.Down
	= do
		-- non-freeglut doesn't like this
		-- GLUT.leaveMainLoop
		
		System.exitWith System.ExitSuccess

	| otherwise
	= return ()