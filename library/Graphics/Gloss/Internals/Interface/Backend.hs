{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}

module Graphics.Gloss.Internals.Interface.Backend
  ( module Graphics.Gloss.Internals.Interface.Backend.Types
#ifdef WITHGLUT
  , module Graphics.Gloss.Internals.Interface.Backend.GLUT
#endif
  , defaultBackendState
  )
where

import Graphics.Gloss.Internals.Interface.Backend.Types
  (Backend(..),Key(..),MouseButton(..),KeyState(..),SpecialKey(..),Modifiers(..),IORef)
#ifdef WITHGLUT
import Graphics.Gloss.Internals.Interface.Backend.GLUT
#endif

#ifdef WITHGLUT
defaultBackendState :: GLUTState
#else
#error No default backend defined
#endif
defaultBackendState = initBackendState
