{-# OPTIONS_HADDOCK hide #-}

-- |    The main display function.
module  Graphics.Gloss.Internals.Interface.Window
        ( createWindow )
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Color
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Debug
import Graphics.Rendering.OpenGL                        (($=))
import qualified Graphics.Rendering.OpenGL.GL           as GL
import Data.IORef (IORef, newIORef)
import Control.Monad

-- | Open a window and use the supplied callbacks to handle window events.
createWindow
        :: Backend a
        => a
        -> Display
        -> Color                -- ^ Color to use when clearing.
        -> [Callback]           -- ^ Callbacks to use.
        -> (IORef a -> IO ())   -- ^ Give the backend back to the caller before
                                --   entering the main loop.
        -> IO ()

createWindow
        backend
        display
        clearColor
        callbacks
        eatBackend
 = do
        -- Turn this on to spew debugging info to stdout
        let debug       = False

        -- Initialize backend state
        backendStateRef <- newIORef backend

        when debug
         $ do   putStr  $ "* displayInWindow\n"

        -- Intialize backend
        initializeBackend backendStateRef debug

        -- Here we go!
        when debug
         $ do   putStr  $ "* c window\n\n"

        -- Open window
        openWindow backendStateRef display

        -- Setup callbacks
        installDisplayCallback     backendStateRef callbacks
        installWindowCloseCallback backendStateRef
        installReshapeCallback     backendStateRef callbacks
        installKeyMouseCallback    backendStateRef callbacks
        installMotionCallback      backendStateRef callbacks
        installIdleCallback        backendStateRef callbacks

        -- we don't need the depth buffer for 2d.
        GL.depthFunc    $= Just GL.Always

        -- always clear the buffer to white
        GL.clearColor   $= glColor4OfColor clearColor

        -- Dump some debugging info
        when debug
         $ do   dumpBackendState backendStateRef
                dumpFramebufferState
                dumpFragmentState

        eatBackend backendStateRef

        when debug
         $ do   putStr  $ "* entering mainloop..\n"

        -- Start the main backend loop
        runMainLoop backendStateRef

        when debug
         $      putStr  $ "* all done\n"

