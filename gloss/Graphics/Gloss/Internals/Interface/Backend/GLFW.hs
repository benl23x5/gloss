{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}

-- | Support for using GLFW as the window manager backend.
module Graphics.Gloss.Internals.Interface.Backend.GLFW
        (GLFWState)
where
import Data.IORef
import Data.Char                           (toLower)
import Control.Monad
import Graphics.Gloss.Data.Display
import Graphics.UI.GLFW                    (WindowValue(..))
import qualified Graphics.UI.GLFW          as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Control.Exception         as X

-- [Note: FreeGlut]
-- ~~~~~~~~~~~~~~~~
-- We use GLUT for font rendering.
--   On freeglut-based installations (usually linux) we need to explicitly
--   initialize GLUT before we can use any of it's functions.
--
---  We also need to deinitialize (exit) GLUT when we close the GLFW
--   window, otherwise opening a gloss window again from GHCi will crash. 
--   For the OS X and Windows version of GLUT there are no such restrictions.
--
--   We assume also assume that only linux installations use freeglut.
--
#ifdef linux_HOST_OS
import qualified Graphics.UI.GLUT          as GLUT
#endif

import Graphics.Gloss.Internals.Interface.Backend.Types

-- | State of the GLFW backend library.
data GLFWState
        = GLFWState
        { -- | Status of Ctrl, Alt or Shift (Up or Down?)
          modifiers     :: Modifiers

        -- | Latest mouse position
        , mousePosition :: (Int,Int)

        -- | Latest mousewheel position
        , mouseWheelPos :: Int

        -- | Does the screen need to be redrawn?
        , dirtyScreen   :: Bool

        -- | Action that draws on the screen
        , display       :: IO ()

        -- | Action perforrmed when idling
        , idle          :: IO ()
        }


-- | Initial GLFW state.
glfwStateInit :: GLFWState
glfwStateInit
        = GLFWState
        { modifiers      = Modifiers Up Up Up
        , mousePosition = (0, 0)
        , mouseWheelPos = 0
        , dirtyScreen   = True
        , display       = return ()
        , idle          = return () }



instance Backend GLFWState where
        initBackendState           = glfwStateInit
        initializeBackend          = initializeGLFW
        exitBackend                = exitGLFW
        openWindow                 = openWindowGLFW
        dumpBackendState           = dumpStateGLFW
        installDisplayCallback     = installDisplayCallbackGLFW
        installWindowCloseCallback = installWindowCloseCallbackGLFW
        installReshapeCallback     = installReshapeCallbackGLFW
        installKeyMouseCallback    = installKeyMouseCallbackGLFW
        installMotionCallback      = installMotionCallbackGLFW
        installIdleCallback        = installIdleCallbackGLFW
        runMainLoop                = runMainLoopGLFW
        postRedisplay              = postRedisplayGLFW
        getWindowDimensions        = (\_     -> GLFW.getWindowDimensions)
        elapsedTime                = (\_     -> GLFW.getTime)
        sleep                      = (\_ sec -> GLFW.sleep sec)


-- Initialise -----------------------------------------------------------------
-- | Initialise the GLFW backend.
initializeGLFW :: IORef GLFWState -> Bool-> IO ()
initializeGLFW _ debug
 = do
        _                   <- GLFW.initialize
        glfwVersion         <- GLFW.getGlfwVersion

#ifdef linux_HOST_OS
-- See [Note: FreeGlut] for why we need this.
        (_progName, _args)  <- GLUT.getArgsAndInitialize
#endif

        when debug
         $ putStr  $ "  glfwVersion        = " ++ show glfwVersion   ++ "\n"


-- Exit -----------------------------------------------------------------------
-- | Tell the GLFW backend to close the window and exit.
exitGLFW :: IORef GLFWState -> IO ()
exitGLFW _
 = do
#ifdef linux_HOST_OS
-- See [Note: FreeGlut] on why we exit GLUT for Linux
        GLUT.exit
#endif
        GLFW.closeWindow


-- Open Window ----------------------------------------------------------------
-- | Open a new window.
openWindowGLFW 
        :: IORef GLFWState
        -> Display
        -> IO ()

openWindowGLFW _ (InWindow title (sizeX, sizeY) pos)
 = do   _ <- GLFW.openWindow
                GLFW.defaultDisplayOptions
                 { GLFW.displayOptions_width        = sizeX
                 , GLFW.displayOptions_height       = sizeY
                 , GLFW.displayOptions_displayMode  = GLFW.Window }
        
        uncurry GLFW.setWindowPosition pos
        GLFW.setWindowTitle title

        -- Try to enable sync-to-vertical-refresh by setting the number 
        -- of buffer swaps per vertical refresh to 1.
        GLFW.setWindowBufferSwapInterval 1

openWindowGLFW _ (FullScreen (sizeX, sizeY))
 = do   _ <- GLFW.openWindow
                GLFW.defaultDisplayOptions
                 { GLFW.displayOptions_width        = sizeX
                 , GLFW.displayOptions_height       = sizeY
                 , GLFW.displayOptions_displayMode  = GLFW.Fullscreen }
        
        -- Try to enable sync-to-vertical-refresh by setting the number 
        -- of buffer swaps per vertical refresh to 1.
        GLFW.setWindowBufferSwapInterval 1
        GLFW.enableMouseCursor

-- Dump State -----------------------------------------------------------------
-- | Print out the internal GLFW state.
dumpStateGLFW :: IORef a -> IO ()
dumpStateGLFW _
 = do   (ww,wh)     <- GLFW.getWindowDimensions

        r           <- GLFW.getWindowValue NumRedBits
        g           <- GLFW.getWindowValue NumGreenBits
        b           <- GLFW.getWindowValue NumBlueBits
        a           <- GLFW.getWindowValue NumAlphaBits
        let rgbaBD  = [r,g,b,a]

        depthBD     <- GLFW.getWindowValue NumDepthBits

        ra          <- GLFW.getWindowValue NumAccumRedBits
        ga          <- GLFW.getWindowValue NumAccumGreenBits
        ba          <- GLFW.getWindowValue NumAccumBlueBits
        aa          <- GLFW.getWindowValue NumAccumAlphaBits
        let accumBD = [ra,ga,ba,aa]

        stencilBD   <- GLFW.getWindowValue NumStencilBits

        auxBuffers  <- GLFW.getWindowValue NumAuxBuffers

        fsaaSamples <- GLFW.getWindowValue NumFsaaSamples

        putStr  $ "* dumpGlfwState\n"
                ++ " windowWidth  = " ++ show ww          ++ "\n"
                ++ " windowHeight = " ++ show wh          ++ "\n"
                ++ " depth rgba   = " ++ show rgbaBD      ++ "\n"
                ++ " depth        = " ++ show depthBD     ++ "\n"
                ++ " accum        = " ++ show accumBD     ++ "\n"
                ++ " stencil      = " ++ show stencilBD   ++ "\n"
                ++ " aux Buffers  = " ++ show auxBuffers  ++ "\n"
                ++ " FSAA Samples = " ++ show fsaaSamples ++ "\n"
                ++ "\n"


-- Display Callback -----------------------------------------------------------
-- | Callback for when GLFW needs us to redraw the contents of the window.
installDisplayCallbackGLFW
        :: IORef GLFWState -> [Callback] -> IO ()

installDisplayCallbackGLFW stateRef callbacks
 =  modifyIORef stateRef
        $ \s -> s { display = callbackDisplay stateRef callbacks }


callbackDisplay
        :: IORef GLFWState -> [Callback]
        -> IO ()

callbackDisplay stateRef callbacks
 = do  -- clear the display
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

        -- get the display callbacks from the chain
        let funs  = [f stateRef | (Display f) <- callbacks]
        sequence_ funs

        return ()


-- Close Callback -------------------------------------------------------------
-- | Callback for when the user closes the window.
--   We can do some cleanup here.
installWindowCloseCallbackGLFW 
        :: IORef GLFWState -> IO ()

installWindowCloseCallbackGLFW _
 = GLFW.setWindowCloseCallback 
 $ do
#ifdef linux_HOST_OS
-- See [Note: FreeGlut] for why we need this.
        GLUT.exit
#endif
        return True


-- Reshape --------------------------------------------------------------------
-- | Callback for when the user reshapes the window.
installReshapeCallbackGLFW
        :: Backend a
        => IORef a -> [Callback] -> IO ()

installReshapeCallbackGLFW stateRef callbacks
        = GLFW.setWindowSizeCallback (callbackReshape stateRef callbacks)

callbackReshape 
        :: Backend a
        => IORef a -> [Callback]
        -> Int -> Int
        -> IO ()

callbackReshape glfwState callbacks sizeX sizeY
  = sequence_
  $ map   (\f -> f (sizeX, sizeY))
    [f glfwState | Reshape f  <- callbacks]


-- KeyMouse -----------------------------------------------------------------------
-- | Callbacks for when the user presses a key or moves / clicks the mouse.
--   This is a bit verbose because we have to do impedence matching between
--   GLFW's event system, and the one use by Gloss which was originally
--   based on GLUT. The main problem is that GLUT only provides a single callback
--   slot for character keys, arrow keys, mouse buttons and mouse wheel movement, 
--   while GLFW provides a single slot for each.
--
installKeyMouseCallbackGLFW
        :: IORef GLFWState -> [Callback]
        -> IO ()

installKeyMouseCallbackGLFW stateRef callbacks
 = do   GLFW.setKeyCallback         $ (callbackKeyboard    stateRef callbacks)
        GLFW.setCharCallback        $ (callbackChar        stateRef callbacks)
        GLFW.setMouseButtonCallback $ (callbackMouseButton stateRef callbacks)
        GLFW.setMouseWheelCallback  $ (callbackMouseWheel  stateRef callbacks)


-- GLFW calls this on a non-character keyboard action.
callbackKeyboard 
        :: IORef GLFWState -> [Callback]
        -> GLFW.Key -> Bool
        -> IO ()

callbackKeyboard stateRef callbacks key keystate
 = do   (modsSet, GLFWState mods pos _ _ _ _)
                <- setModifiers stateRef key keystate     
        let key'      = fromGLFW key
        let keystate' = if keystate then Down else Up
        let isCharKey (Char _) = True
            isCharKey _        = False

        -- Call the Gloss KeyMouse actions with the new state.
        unless (modsSet || isCharKey key' && keystate)
         $ sequence_ 
         $ map  (\f -> f key' keystate' mods pos)
                [f stateRef | KeyMouse f <- callbacks]


setModifiers 
        :: IORef GLFWState
        -> GLFW.Key -> Bool
        -> IO (Bool, GLFWState)

setModifiers stateRef key pressed
 = do   glfwState <- readIORef stateRef
        let mods  = modifiers glfwState
        let mods' = case key of
                GLFW.KeyLeftShift -> mods {shift = if pressed then Down else Up}
                GLFW.KeyLeftCtrl  -> mods {ctrl  = if pressed then Down else Up}
                GLFW.KeyLeftAlt   -> mods {alt   = if pressed then Down else Up}
                _                 -> mods

        if (mods' /= mods)
         then do
                let glfwState' = glfwState {modifiers = mods'}
                writeIORef stateRef glfwState'
                return (True, glfwState')
         else return (False, glfwState)


-- GLFW calls this on a when the user presses or releases a character key.
callbackChar 
        :: IORef GLFWState -> [Callback]
        -> Char -> Bool -> IO ()

callbackChar stateRef callbacks char keystate
 = do   (GLFWState mods pos _ _ _ _) <- readIORef stateRef
        let key'      = charToSpecial char
        -- Only key presses of characters are passed to this callback,
        -- character key releases are caught by the 'keyCallback'. This is an
        -- intentional feature of GLFW. What this means that a key press of
        -- the '>' char  (on a US Intl keyboard) is captured by this callback,
        -- but a release is captured as a '.' with the shift-modifier in the
        -- keyCallback.
        let keystate' = if keystate then Down else Up

        -- Call all the Gloss KeyMouse actions with the new state.
        sequence_ 
         $ map  (\f -> f key' keystate' mods pos) 
                [f stateRef | KeyMouse f <- callbacks]


-- GLFW calls on this when the user clicks or releases a mouse button.
callbackMouseButton 
        :: IORef GLFWState -> [Callback]
        -> GLFW.MouseButton
        -> Bool
        -> IO ()

callbackMouseButton stateRef callbacks key keystate
 = do   (GLFWState mods pos _ _ _ _) <- readIORef stateRef
        let key'      = fromGLFW key
        let keystate' = if keystate then Down else Up

        -- Call all the Gloss KeyMouse actions with the new state.
        sequence_ 
         $ map  (\f -> f key' keystate' mods pos)
                [f stateRef | KeyMouse f <- callbacks]


-- GLFW calls on this when the user moves the mouse wheel.
callbackMouseWheel
        :: IORef GLFWState -> [Callback]
        -> Int
        -> IO ()

callbackMouseWheel stateRef callbacks w
 = do   (key, keystate)  <- setMouseWheel stateRef w
        (GLFWState mods pos _ _ _ _) <- readIORef stateRef

        -- Call all the Gloss KeyMouse actions with the new state.
        sequence_ 
         $ map  (\f -> f key keystate mods pos)
                [f stateRef | KeyMouse f <- callbacks]

setMouseWheel
        :: IORef GLFWState
        -> Int
        -> IO (Key, KeyState)

setMouseWheel stateRef w
 = do   glfwState <- readIORef stateRef
        writeIORef stateRef $ glfwState {mouseWheelPos = w}
        case compare w (mouseWheelPos glfwState) of
                LT -> return (MouseButton WheelDown , Down)
                GT -> return (MouseButton WheelUp   , Down)
                EQ -> return (SpecialKey  KeyUnknown, Up  )


-- Motion Callback ------------------------------------------------------------
-- | Callback for when the user moves the mouse.
installMotionCallbackGLFW 
        :: IORef GLFWState -> [Callback]
        -> IO ()

installMotionCallbackGLFW stateRef callbacks
        = GLFW.setMousePositionCallback $ (callbackMotion stateRef callbacks)

callbackMotion 
        :: IORef GLFWState -> [Callback]
        -> Int -> Int
        -> IO ()
callbackMotion stateRef callbacks x y
 = do   pos <- setMousePos stateRef x y

        -- Call all the Gloss Motion actions with the new state.
        sequence_ 
         $ map  (\f -> f pos)
                [f stateRef | Motion f <- callbacks]

setMousePos
        :: IORef GLFWState
        -> Int -> Int
        -> IO (Int, Int)
setMousePos stateRef x y
 = do   let pos = (x,y)
        modifyIORef stateRef (\s -> s {mousePosition = pos})
        return pos


-- Idle Callback --------------------------------------------------------------
-- | Callback for when GLFW has finished its jobs and it's time for us to do
--   something for our application.
installIdleCallbackGLFW
        :: IORef GLFWState -> [Callback]
        -> IO ()

installIdleCallbackGLFW stateRef callbacks 
        = modifyIORef stateRef (\s -> s {idle = callbackIdle stateRef callbacks})

callbackIdle 
        :: IORef GLFWState -> [Callback]
        -> IO ()

callbackIdle stateRef callbacks
        = sequence_
        $ [f stateRef | Idle f <- callbacks]


-- Main Loop ------------------------------------------------------------------
runMainLoopGLFW
        :: IORef GLFWState
        -> IO ()

runMainLoopGLFW stateRef 
 = X.catch go exit
 where
  exit :: X.SomeException -> IO ()
  exit e = print e >> exitGLFW stateRef

  go   :: IO ()
  go 
   = do windowIsOpen <- GLFW.windowIsOpen
        when windowIsOpen 
         $ do  GLFW.pollEvents
               dirty <- fmap dirtyScreen $ readIORef stateRef

               when dirty
                $ do   s <- readIORef stateRef
                       display s
                       GLFW.swapBuffers

               modifyIORef stateRef $ \s -> s { dirtyScreen = False }
               (readIORef stateRef) >>= (\s -> idle s)
               GLFW.sleep 0.001
               runMainLoopGLFW stateRef


-- Redisplay ------------------------------------------------------------------
postRedisplayGLFW 
        :: IORef GLFWState
        -> IO ()

postRedisplayGLFW stateRef
        = modifyIORef stateRef
        $ \s -> s { dirtyScreen = True }


-- Key Code Conversion --------------------------------------------------------
class GLFWKey a where
  fromGLFW :: a -> Key

instance GLFWKey GLFW.Key where
  fromGLFW key 
   = case key of
        GLFW.CharKey c      -> charToSpecial (toLower c)
        GLFW.KeySpace       -> SpecialKey KeySpace
        GLFW.KeyEsc         -> SpecialKey KeyEsc
        GLFW.KeyF1          -> SpecialKey KeyF1
        GLFW.KeyF2          -> SpecialKey KeyF2
        GLFW.KeyF3          -> SpecialKey KeyF3
        GLFW.KeyF4          -> SpecialKey KeyF4
        GLFW.KeyF5          -> SpecialKey KeyF5
        GLFW.KeyF6          -> SpecialKey KeyF6
        GLFW.KeyF7          -> SpecialKey KeyF7
        GLFW.KeyF8          -> SpecialKey KeyF8
        GLFW.KeyF9          -> SpecialKey KeyF9
        GLFW.KeyF10         -> SpecialKey KeyF10
        GLFW.KeyF11         -> SpecialKey KeyF11
        GLFW.KeyF12         -> SpecialKey KeyF12
        GLFW.KeyF13         -> SpecialKey KeyF13
        GLFW.KeyF14         -> SpecialKey KeyF14
        GLFW.KeyF15         -> SpecialKey KeyF15
        GLFW.KeyF16         -> SpecialKey KeyF16
        GLFW.KeyF17         -> SpecialKey KeyF17
        GLFW.KeyF18         -> SpecialKey KeyF18
        GLFW.KeyF19         -> SpecialKey KeyF19
        GLFW.KeyF20         -> SpecialKey KeyF20
        GLFW.KeyF21         -> SpecialKey KeyF21
        GLFW.KeyF22         -> SpecialKey KeyF22
        GLFW.KeyF23         -> SpecialKey KeyF23
        GLFW.KeyF24         -> SpecialKey KeyF24
        GLFW.KeyF25         -> SpecialKey KeyF25
        GLFW.KeyUp          -> SpecialKey KeyUp
        GLFW.KeyDown        -> SpecialKey KeyDown
        GLFW.KeyLeft        -> SpecialKey KeyLeft
        GLFW.KeyRight       -> SpecialKey KeyRight
        GLFW.KeyTab         -> SpecialKey KeyTab
        GLFW.KeyEnter       -> SpecialKey KeyEnter
        GLFW.KeyBackspace   -> SpecialKey KeyBackspace
        GLFW.KeyInsert      -> SpecialKey KeyInsert
        GLFW.KeyDel         -> SpecialKey KeyDelete
        GLFW.KeyPageup      -> SpecialKey KeyPageUp
        GLFW.KeyPagedown    -> SpecialKey KeyPageDown
        GLFW.KeyHome        -> SpecialKey KeyHome
        GLFW.KeyEnd         -> SpecialKey KeyEnd
        GLFW.KeyPad0        -> SpecialKey KeyPad0
        GLFW.KeyPad1        -> SpecialKey KeyPad1
        GLFW.KeyPad2        -> SpecialKey KeyPad2
        GLFW.KeyPad3        -> SpecialKey KeyPad3
        GLFW.KeyPad4        -> SpecialKey KeyPad4
        GLFW.KeyPad5        -> SpecialKey KeyPad5
        GLFW.KeyPad6        -> SpecialKey KeyPad6
        GLFW.KeyPad7        -> SpecialKey KeyPad7
        GLFW.KeyPad8        -> SpecialKey KeyPad8
        GLFW.KeyPad9        -> SpecialKey KeyPad9
        GLFW.KeyPadDivide   -> SpecialKey KeyPadDivide
        GLFW.KeyPadMultiply -> SpecialKey KeyPadMultiply
        GLFW.KeyPadSubtract -> SpecialKey KeyPadSubtract
        GLFW.KeyPadAdd      -> SpecialKey KeyPadAdd
        GLFW.KeyPadDecimal  -> SpecialKey KeyPadDecimal
        GLFW.KeyPadEqual    -> Char '='
        GLFW.KeyPadEnter    -> SpecialKey KeyPadEnter
        _                   -> SpecialKey KeyUnknown


-- | Convert char keys to special keys to work around a bug in 
--   GLFW 2.7. On OS X, GLFW sometimes registers special keys as char keys,
--   so we convert them back here.
--   GLFW 2.7 is current as of Nov 2011, and is shipped with the Hackage
--   binding GLFW-b 0.2.*
charToSpecial :: Char -> Key
charToSpecial c = case (fromEnum c) of
        32    -> SpecialKey KeySpace
        63232 -> SpecialKey KeyUp
        63233 -> SpecialKey KeyDown
        63234 -> SpecialKey KeyLeft
        63235 -> SpecialKey KeyRight
        63236 -> SpecialKey KeyF1
        63237 -> SpecialKey KeyF2
        63238 -> SpecialKey KeyF3
        63239 -> SpecialKey KeyF4
        63240 -> SpecialKey KeyF5
        63241 -> SpecialKey KeyF6
        63242 -> SpecialKey KeyF7
        63243 -> SpecialKey KeyF8
        63244 -> SpecialKey KeyF9
        63245 -> SpecialKey KeyF10
        63246 -> SpecialKey KeyF11
        63247 -> SpecialKey KeyF12
        63248 -> SpecialKey KeyF13
        63272 -> SpecialKey KeyDelete
        63273 -> SpecialKey KeyHome
        63275 -> SpecialKey KeyEnd
        63276 -> SpecialKey KeyPageUp
        63277 -> SpecialKey KeyPageDown
        _     -> Char c

instance GLFWKey GLFW.MouseButton where
  fromGLFW mouse
   = case mouse of
        GLFW.MouseButton0 -> MouseButton LeftButton
        GLFW.MouseButton1 -> MouseButton RightButton
        GLFW.MouseButton2 -> MouseButton MiddleButton
        GLFW.MouseButton3 -> MouseButton $ AdditionalButton 3
        GLFW.MouseButton4 -> MouseButton $ AdditionalButton 4
        GLFW.MouseButton5 -> MouseButton $ AdditionalButton 5
        GLFW.MouseButton6 -> MouseButton $ AdditionalButton 6
        GLFW.MouseButton7 -> MouseButton $ AdditionalButton 7
