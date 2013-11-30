{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module Graphics.Gloss.Internals.Interface.Backend.GLiOS
        (GLiOSState, backendLog)
where

import           Control.Concurrent
import           Data.IORef
import qualified System.Exit               as System
import           Graphics.Rendering.OpenGL as GL
import           Graphics.Gloss.Internals.Interface.Backend.Types
import           Foreign
import           Foreign.C
import System.IO (hPutStrLn, stderr)

foreign import ccall gliosGetWindowWidth     :: CInt
foreign import ccall gliosGetWindowHeight    :: CInt
foreign import ccall gliosSetDisplayCallback :: FunPtr (IO ()) -> IO ()
foreign import ccall gliosInit               :: IO ()
foreign import ccall gliosOpenWindow         :: IO ()
foreign import ccall gliosElapsedTime        :: IO CDouble
foreign import ccall safe gliosMainLoop      :: IO ()
foreign import ccall gliosPostRedisplay      :: IO ()
foreign import ccall "wrapper" mkFunPtr      :: IO () -> IO (FunPtr (IO ()))
foreign import ccall gliosLog                :: CString -> IO ()

-- | We don't maintain any state information for the GLiOS backend,
--   so this data type is empty.
data GLiOSState
        = GLiOSState

instance Backend GLiOSState where
        initBackendState           = GLiOSState
        initializeBackend  _ _     = gliosInit

        exitBackend                = (\_ -> System.exitWith System.ExitSuccess)

        openWindow  _ _            = return ()
        dumpBackendState           = (\_ -> return ())
        installDisplayCallback     = installDisplayCallbackGLiOS

        -- We can ask for this in freeglut, but it doesn't seem to work :(.
        -- (\_ -> GLUT.actionOnWindowClose $= GLUT.MainLoopReturns)
        installWindowCloseCallback = (\_ -> return ())

        installReshapeCallback     = (\_ _ -> return ())
        installKeyMouseCallback    = (\_ _ -> return ())
        installMotionCallback      = (\_ _ -> return ())
        installIdleCallback        = (\_ _ -> return ())

        -- Call the GLUT mainloop.
        -- This function will return when something calls GLUT.leaveMainLoop
        runMainLoop _
         =  gliosMainLoop

        postRedisplay _
         =      gliosPostRedisplay

        getWindowDimensions _
         = gliosGetWindowDimensions

        elapsedTime _
         = do   t       <- gliosElapsedTime
                return $ (fromRational . toRational $ t)

        sleep _ sec
         = do threadDelay (round $ sec * 1000000)

-- | Logs messages to ~/Library/Logs/iOS\ Simulator/<version>/system.log
backendLog :: String -> IO ()
backendLog s = do
  cstr <- newCString s
  gliosLog cstr

-------------------

gliosGetWindowDimensions :: IO (Int, Int)
gliosGetWindowDimensions =
  return (fromIntegral gliosGetWindowWidth, fromIntegral gliosGetWindowHeight)

installDisplayCallbackGLiOS
        :: IORef GLiOSState -> [Callback]
        -> IO ()
installDisplayCallbackGLiOS ref callbacks = do
    ptr <- mkFunPtr $ callbackDisplay ref callbacks
    gliosSetDisplayCallback ptr

callbackDisplay
        :: IORef GLiOSState -> [Callback]
        -> IO ()

callbackDisplay ref callbacks
 = do   -- clear the display
        GL.clear [ GL.ColorBuffer, GL.DepthBuffer]
        GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)
        -- get the display callbacks from the chain
        let funs  = [f ref | (Display f) <- callbacks]
        sequence_ funs
        GL.flush
        return ()