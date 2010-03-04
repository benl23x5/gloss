{-# OPTIONS_HADDOCK hide #-}

-- | Event callbacks.
module Graphics.Gloss.Internals.Interface.Callback where

import qualified Graphics.UI.GLUT	as GLUT

-- | Holds callback functions.
data Callback
	= Display	GLUT.DisplayCallback
	| KeyMouse	GLUT.KeyboardMouseCallback
	| Idle		GLUT.IdleCallback
	| Motion	GLUT.MotionCallback	
	| Reshape	GLUT.ReshapeCallback
