{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_HADDOCK hide #-}
module Graphics.Gloss.Internals.Interface.Backend.GLiOS
        (GLiOSState, backendLog)
where

import           Control.Concurrent
import           Data.IORef
import qualified System.Exit               as System
import           Graphics.Rendering.OpenGL as GL
import           Graphics.Gloss.Internals.Interface.Backend.Types
import           Foreign   (FunPtr)
import           Foreign.C
import qualified Data.Map as Map ( empty, lookup, insert, delete )
import           Data.Map ( Map )
import           System.IO.Unsafe

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

foreign export ccall gliosHookMouseDown      :: CInt -> CInt -> IO ()
foreign export ccall gliosHookMotion         :: CInt -> CInt -> IO ()

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
        installKeyMouseCallback    = installKeyMouseCallbackGLiOS
        installMotionCallback      = installMotionCallbackGLiOS
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
-- This seems to be a common Haskell hack nowadays: A plain old global variable
-- with an associated mutator. Perhaps some language/library support is needed?

data CallbackType =
            MouseCallbackType
          | MotionCallbackType
--          DisplayCallbackType
--        | KeyCallbackType
--        | IdleCallbackType

--        | ReshapeCallbackType
       deriving (Eq, Ord)

data GLiOSCallback =
    GLiOSMouseCallback ((Int, Int) -> IO ())
  | GLiOSMotionCallback ((Int,Int) -> IO ())

type CallbackTable = Map CallbackType GLiOSCallback

{-# NOINLINE theCallbackTable #-}
theCallbackTable :: IORef CallbackTable
theCallbackTable = unsafePerformIO (newIORef emptyCallbackTable)

getCallbackTable :: IO CallbackTable
getCallbackTable = readIORef theCallbackTable

modifyCallbackTable :: (CallbackTable -> CallbackTable) -> IO ()
modifyCallbackTable = modifyIORef theCallbackTable

emptyCallbackTable :: CallbackTable
emptyCallbackTable = Map.empty

lookupInCallbackTable :: CallbackType -> IO (Maybe GLiOSCallback)
lookupInCallbackTable typ =
   fmap (Map.lookup typ) getCallbackTable

deleteFromCallbackTable :: CallbackType -> IO ()
deleteFromCallbackTable callbackID =
   modifyCallbackTable (Map.delete callbackID)

addToCallbackTable :: CallbackType -> GLiOSCallback -> IO ()
addToCallbackTable callbackID funPtr =
   modifyCallbackTable (Map.insert callbackID funPtr)

-------------------
--
-- GLiOS Hook funtions.
--
-- Hook functions are Haskell functions that are called by GLiOS (from C to Haskell, rather than
-- the other way around)
--
gliosHookMouseDown :: CInt -> CInt -> IO ()
gliosHookMouseDown posX posY = do
  mbCallback <- lookupInCallbackTable MouseCallbackType
  case mbCallback of
    Just (GLiOSMouseCallback f) -> f (fromIntegral posX, fromIntegral posY)
    _                           -> return ()


gliosHookMotion :: CInt -> CInt -> IO ()
gliosHookMotion posX posY = do
  mbCallback <- lookupInCallbackTable MotionCallbackType
  case mbCallback of
    Just (GLiOSMotionCallback f) -> f (fromIntegral posX, fromIntegral posY)
    _                            -> return ()

-------------------

installKeyMouseCallbackGLiOS :: IORef GLiOSState -> [Callback] -> IO ()
installKeyMouseCallbackGLiOS ref callbacks = do
  addToCallbackTable MouseCallbackType (GLiOSMouseCallback callbackMouseDown)
  where
    callbackMouseDown :: (Int, Int) -> IO ()
    callbackMouseDown pos = sequence_ $
                map (\f -> f (MouseButton LeftButton) Down
                             (Modifiers { shift = Up, ctrl = Up, alt = Up}) pos)
                [f ref | KeyMouse f <- callbacks]

installMotionCallbackGLiOS :: IORef GLiOSState -> [Callback] -> IO ()
installMotionCallbackGLiOS ref callbacks = do
  addToCallbackTable MotionCallbackType (GLiOSMotionCallback motion)
  where
    motion :: (Int, Int) -> IO ()
    motion (posX, posY) = do
      let pos = (fromEnum posX, fromEnum posY)
      sequence_ $ map  (\f -> f pos) [f ref | Motion f <- callbacks]



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