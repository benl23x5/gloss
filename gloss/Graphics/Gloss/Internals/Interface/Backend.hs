{-# LANGUAGE CPP #-}

-- Import window managed backend specific modules.
-- We need to use #ifdef here because if the backend library hasn't been installed
-- then we won't be able to build it, so it can't be in the import list.
module Graphics.Gloss.Internals.Interface.Backend
        ( module Graphics.Gloss.Internals.Interface.Backend.Types
#ifdef WITHGLFW
        , module Graphics.Gloss.Internals.Interface.Backend.GLFW
#endif
#ifdef WITHGLUT
        , module Graphics.Gloss.Internals.Interface.Backend.GLUT
#endif
        , defaultBackendState)
where

import Graphics.Gloss.Internals.Interface.Backend.Types

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
