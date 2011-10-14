{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}

module Graphics.Gloss.Internals.Interface.Backend
  ( module Graphics.Gloss.Internals.Interface.Backend.Types
#ifdef WITHGLFW
  , module Graphics.Gloss.Internals.Interface.Backend.GLFW
#endif
#ifdef WITHGLUT
  , module Graphics.Gloss.Internals.Interface.Backend.GLUT
#endif
  , defaultBackendState
  )
where

import Graphics.Gloss.Internals.Interface.Backend.Types
  (Backend(..),Key(..),MouseButton(..),KeyState(..),SpecialKey(..),Modifiers(..),IORef)
#ifdef WITHGLFW
import Graphics.Gloss.Internals.Interface.Backend.GLFW
#endif
#ifdef WITHGLUT
import Graphics.Gloss.Internals.Interface.Backend.GLUT
#endif

#ifdef WITHGLUT
defaultBackendState :: GLUTState
#elif  WITHGLFW
defaultBackendState :: GLFWState
#else
#error No default backend defined
#endif
defaultBackendState = initBackendState
