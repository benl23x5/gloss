{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}

-- | Support for using GLFW as the window manager backend.
module Graphics.Gloss.Internals.Interface.Backend.GLFW
        (GLFWState)
where
import Data.IORef
import Data.Char                           (toLower)
import Data.Maybe                          (fromJust)
import Control.Concurrent
import Control.Monad
import Graphics.Gloss.Data.Display
-- import Graphics.UI.GLFW                    (WindowValue(..))
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

        -- | The Window Handle
        , winHdl        :: GLFW.Window 
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
        getWindowDimensions        = (\ref   -> windowHandle ref >>= \win -> GLFW.getWindowSize win)
        elapsedTime                = (\_     -> GLFW.getTime >>= \mt -> return $ fromJust mt)
        sleep                      = (\_ sec -> threadDelay (floor (sec * fromIntegral 1000000))) --GLFW.sleep sec)

-- Initialise -----------------------------------------------------------------
-- | Initialise the GLFW backend.
initializeGLFW :: IORef GLFWState -> Bool-> IO ()
initializeGLFW _ debug
 = do
        _                   <- GLFW.init
        glfwVersion         <- GLFW.getVersion

#ifdef linux_HOST_OS
-- See [Note: FreeGlut] for why we need this.
        (_progName, _args)  <- GLUT.getArgsAndInitialize
#endif

        when debug
         $ putStr  $ "  glfwVersion        = " ++ show glfwVersion   ++ "\n"


-- Exit -----------------------------------------------------------------------
-- | Tell the GLFW backend to close the window and exit.
exitGLFW :: IORef GLFWState -> IO ()
exitGLFW ref
 = do
#ifdef linux_HOST_OS
-- See [Note: FreeGlut] on why we exit GLUT for Linux
        GLUT.exit
#endif
        win <- windowHandle ref
        GLFW.destroyWindow win


-- Open Window ----------------------------------------------------------------
-- | Open a new window.
openWindowGLFW
        :: IORef GLFWState
        -> Display
        -> IO ()

openWindowGLFW ref (InWindow title (sizeX, sizeY) pos)
 = do   win <- GLFW.createWindow 
                sizeX
                sizeY
                title
                Nothing
                Nothing 

        modifyIORef' ref (\s -> s { winHdl = fromJust win})
        uncurry (GLFW.setWindowPos (fromJust win)) pos

        -- Try to enable sync-to-vertical-refresh by setting the number
        -- of buffer swaps per vertical refresh to 1.
        GLFW.swapInterval 1

openWindowGLFW ref FullScreen
 = do   mon   <- GLFW.getPrimaryMonitor
        vmode <- GLFW.getVideoMode (fromJust mon)

        let sizeX = GLFW.videoModeWidth (fromJust vmode)
        let sizeY = GLFW.videoModeHeight (fromJust vmode)

        win <- GLFW.createWindow 
                sizeX
                sizeY
                ""
                mon
                Nothing 

        modifyIORef' ref (\s -> s { winHdl = fromJust win})

        -- Try to enable sync-to-vertical-refresh by setting the number
        -- of buffer swaps per vertical refresh to 1.
        GLFW.swapInterval 1
        --GLFW.enableMouseCursor
        GLFW.setCursorInputMode (fromJust win) GLFW.CursorInputMode'Normal


windowHandle :: IORef GLFWState -> IO GLFW.Window
windowHandle ref
 = do s <- readIORef ref
      return $ winHdl s

-- Dump State -----------------------------------------------------------------
-- | Print out the internal GLFW state.
dumpStateGLFW :: IORef GLFWState -> IO ()
dumpStateGLFW ref
 = do   win         <- windowHandle ref
        (ww,wh)     <- GLFW.getWindowSize win
        
-- GLFW-b does not provide a general function to query windowHints
-- could be added by adding additional getWindowHint which
-- uses glfwGetWindowAttrib behind the scenes as has been done 
-- already for e.g. getWindowVisible which uses glfwGetWindowAttrib
{-
        r           <- GLFW.getWindowHint NumRedBits
        g           <- GLFW.getWindowHint NumGreenBits
        b           <- GLFW.getWindowHint NumBlueBits
        a           <- GLFW.getWindowHint NumAlphaBits
        let rgbaBD  = [r,g,b,a]

        depthBD     <- GLFW.getWindowHint NumDepthBits

        ra          <- GLFW.getWindowHint NumAccumRedBits
        ga          <- GLFW.getWindowHint NumAccumGreenBits
        ba          <- GLFW.getWindowHint NumAccumBlueBits
        aa          <- GLFW.getWindowHint NumAccumAlphaBits
        let accumBD = [ra,ga,ba,aa]

        stencilBD   <- GLFW.getWindowHint NumStencilBits

        auxBuffers  <- GLFW.getWindowHint NumAuxBuffers

        fsaaSamples <- GLFW.getWindowHint NumFsaaSamples

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
-}

        putStr  $ "* dumpGlfwState\n"
                ++ " windowWidth  = " ++ show ww          ++ "\n"
                ++ " windowHeight = " ++ show wh          ++ "\n"
                ++ "\n"

-- Display Callback -----------------------------------------------------------
-- | Callback for when GLFW needs us to redraw the contents of the window.
installDisplayCallbackGLFW
        :: IORef GLFWState -> [Callback] -> IO ()

installDisplayCallbackGLFW stateRef callbacks
 =  modifyIORef' stateRef $ \s -> s
        { display = callbackDisplay stateRef callbacks }


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

installWindowCloseCallbackGLFW ref
 = do win <- windowHandle ref
      GLFW.setWindowCloseCallback win (Just winClosed)
 where
  winClosed :: GLFW.WindowCloseCallback
  winClosed win = do
#ifdef linux_HOST_OS
-- See [Note: FreeGlut] for why we need this.
        GLUT.exit
#endif
        return ()

-- Reshape --------------------------------------------------------------------
-- | Callback for when the user reshapes the window.
installReshapeCallbackGLFW
        :: IORef GLFWState -> [Callback] -> IO ()

installReshapeCallbackGLFW stateRef callbacks
 = do win <- windowHandle stateRef
      GLFW.setWindowSizeCallback win (Just $ callbackReshape stateRef callbacks)

callbackReshape
        :: IORef GLFWState -> [Callback]
        -> GLFW.WindowSizeCallback -- = Window -> Int -> Int -> IO ()

callbackReshape glfwState callbacks win sizeX sizeY
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
 = do   win <- windowHandle stateRef
        GLFW.setKeyCallback         win (Just $ callbackKeyboard    stateRef callbacks)
        GLFW.setCharCallback        win (Just $ callbackChar        stateRef callbacks)
        GLFW.setMouseButtonCallback win (Just $ callbackMouseButton stateRef callbacks)
        GLFW.setScrollCallback      win (Just $ callbackMouseWheel  stateRef callbacks)


-- GLFW calls this on a non-character keyboard action.
callbackKeyboard
        :: IORef GLFWState -> [Callback]
        -> GLFW.KeyCallback -- = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
        -- -> GLFW.Key -> Bool
        -- -> IO ()

callbackKeyboard stateRef callbacks win key scancode keystateglfw modifiers
 = do   let keystate = keystateglfw == GLFW.KeyState'Pressed
        (modsSet, GLFWState mods pos _ _ _ _ _)
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
                GLFW.Key'LeftShift    -> mods {shift = if pressed then Down else Up}
                GLFW.Key'LeftControl  -> mods {ctrl  = if pressed then Down else Up}
                GLFW.Key'LeftAlt      -> mods {alt   = if pressed then Down else Up}
                _                     -> mods

        if (mods' /= mods)
         then do
                let glfwState' = glfwState {modifiers = mods'}
                writeIORef stateRef glfwState'
                return (True, glfwState')
         else return (False, glfwState)


-- GLFW calls this on a when the user presses or releases a character key.
callbackChar
        :: IORef GLFWState -> [Callback]
        -> GLFW.CharCallback
        -- Window -> Char -> IO ()
        -- -> Char -> Bool -> IO ()

callbackChar stateRef callbacks win char -- keystate
 = do   (GLFWState mods pos _ _ _ _ _) <- readIORef stateRef
        let key'      = charToSpecial char
        -- TODO: is this correct? GLFW does not provide the keystate 
        -- in a character callback, here we asume that its pressed
        let keystate = True 

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
        -> GLFW.MouseButtonCallback -- = Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
        
callbackMouseButton stateRef callbacks win key keystate modifier
 = do   (GLFWState mods pos _ _ _ _ _) <- readIORef stateRef
        let key'      = fromGLFW key
        let keystate' = if keystate == GLFW.MouseButtonState'Pressed then Down else Up

        -- Call all the Gloss KeyMouse actions with the new state.
        sequence_
         $ map  (\f -> f key' keystate' mods pos)
                [f stateRef | KeyMouse f <- callbacks]


-- GLFW calls on this when the user moves the mouse wheel.
callbackMouseWheel
        :: IORef GLFWState -> [Callback]
        -> GLFW.ScrollCallback
        -- -> Int
        -- -> IO ()
-- ScrollCallback = Window -> Double -> Double -> IO ()
callbackMouseWheel stateRef callbacks win x y 
 = do   (key, keystate)  <- setMouseWheel stateRef (floor x)
        (GLFWState mods pos _ _ _ _ _) <- readIORef stateRef

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
 = do win <- windowHandle stateRef
      GLFW.setCursorPosCallback win (Just $ callbackMotion stateRef callbacks)

--CursorPosCallback = Window -> Double -> Double -> IO ()

callbackMotion
        :: IORef GLFWState -> [Callback]
        -> GLFW.CursorPosCallback
callbackMotion stateRef callbacks win x y
 = do   pos <- setMousePos stateRef (floor x) (floor y)

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

        modifyIORef' stateRef $ \s -> s
                { mousePosition = pos }

        return pos


-- Idle Callback --------------------------------------------------------------
-- | Callback for when GLFW has finished its jobs and it's time for us to do
--   something for our application.
installIdleCallbackGLFW
        :: IORef GLFWState -> [Callback]
        -> IO ()

installIdleCallbackGLFW stateRef callbacks
        = modifyIORef' stateRef $ \s -> s
                { idle = callbackIdle stateRef callbacks }

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
   = 
     do win <- windowHandle stateRef
        windowIsOpen <- GLFW.windowShouldClose win
        when windowIsOpen
         $ do  GLFW.pollEvents
               dirty <- fmap dirtyScreen $ readIORef stateRef

               when dirty
                $ do   s <- readIORef stateRef
                       display s
                       GLFW.swapBuffers win

               modifyIORef' stateRef $ \s -> s
                        { dirtyScreen = False }

               (readIORef stateRef) >>= (\s -> idle s)
               threadDelay 1000
               runMainLoopGLFW stateRef


-- Redisplay ------------------------------------------------------------------
postRedisplayGLFW
        :: IORef GLFWState
        -> IO ()

postRedisplayGLFW stateRef
        = modifyIORef' stateRef $ \s -> s
                { dirtyScreen = True }


-- Key Code Conversion --------------------------------------------------------
class GLFWKey a where
  fromGLFW :: a -> Key

instance GLFWKey GLFW.Key where
  fromGLFW key
   = case key of
        GLFW.Key'A	     -> charToSpecial 'a'
        GLFW.Key'B	     -> charToSpecial 'b'
        GLFW.Key'C	     -> charToSpecial 'c'
        GLFW.Key'D	     -> charToSpecial 'd'
        GLFW.Key'E	     -> charToSpecial 'e'
        GLFW.Key'F	     -> charToSpecial 'f'
        GLFW.Key'G	     -> charToSpecial 'g'
        GLFW.Key'H	     -> charToSpecial 'h'
        GLFW.Key'I	     -> charToSpecial 'i'
        GLFW.Key'J	     -> charToSpecial 'j'
        GLFW.Key'K	     -> charToSpecial 'k'
        GLFW.Key'L	     -> charToSpecial 'l'
        GLFW.Key'M	     -> charToSpecial 'm'
        GLFW.Key'N	     -> charToSpecial 'n'
        GLFW.Key'O	     -> charToSpecial 'o'
        GLFW.Key'P	     -> charToSpecial 'p'
        GLFW.Key'Q	     -> charToSpecial 'q'
        GLFW.Key'R	     -> charToSpecial 'r'
        GLFW.Key'S	     -> charToSpecial 's'
        GLFW.Key'T	     -> charToSpecial 't'
        GLFW.Key'U	     -> charToSpecial 'u'
        GLFW.Key'V	     -> charToSpecial 'v'
        GLFW.Key'W	     -> charToSpecial 'w'
        GLFW.Key'X	     -> charToSpecial 'x'
        GLFW.Key'Y	     -> charToSpecial 'y'
        GLFW.Key'Z           -> charToSpecial 'z'
        GLFW.Key'Space       -> SpecialKey KeySpace
        GLFW.Key'Escape      -> SpecialKey KeyEsc
        GLFW.Key'F1          -> SpecialKey KeyF1
        GLFW.Key'F2          -> SpecialKey KeyF2
        GLFW.Key'F3          -> SpecialKey KeyF3
        GLFW.Key'F4          -> SpecialKey KeyF4
        GLFW.Key'F5          -> SpecialKey KeyF5
        GLFW.Key'F6          -> SpecialKey KeyF6
        GLFW.Key'F7          -> SpecialKey KeyF7
        GLFW.Key'F8          -> SpecialKey KeyF8
        GLFW.Key'F9          -> SpecialKey KeyF9
        GLFW.Key'F10         -> SpecialKey KeyF10
        GLFW.Key'F11         -> SpecialKey KeyF11
        GLFW.Key'F12         -> SpecialKey KeyF12
        GLFW.Key'F13         -> SpecialKey KeyF13
        GLFW.Key'F14         -> SpecialKey KeyF14
        GLFW.Key'F15         -> SpecialKey KeyF15
        GLFW.Key'F16         -> SpecialKey KeyF16
        GLFW.Key'F17         -> SpecialKey KeyF17
        GLFW.Key'F18         -> SpecialKey KeyF18
        GLFW.Key'F19         -> SpecialKey KeyF19
        GLFW.Key'F20         -> SpecialKey KeyF20
        GLFW.Key'F21         -> SpecialKey KeyF21
        GLFW.Key'F22         -> SpecialKey KeyF22
        GLFW.Key'F23         -> SpecialKey KeyF23
        GLFW.Key'F24         -> SpecialKey KeyF24
        GLFW.Key'F25         -> SpecialKey KeyF25
        GLFW.Key'Up          -> SpecialKey KeyUp
        GLFW.Key'Down        -> SpecialKey KeyDown
        GLFW.Key'Left        -> SpecialKey KeyLeft
        GLFW.Key'Right       -> SpecialKey KeyRight
        GLFW.Key'Tab         -> SpecialKey KeyTab
        GLFW.Key'Enter       -> SpecialKey KeyEnter
        GLFW.Key'Backspace   -> SpecialKey KeyBackspace
        GLFW.Key'Insert      -> SpecialKey KeyInsert
        GLFW.Key'Delete      -> SpecialKey KeyDelete
        GLFW.Key'PageUp      -> SpecialKey KeyPageUp
        GLFW.Key'PageDown    -> SpecialKey KeyPageDown
        GLFW.Key'Home        -> SpecialKey KeyHome
        GLFW.Key'End         -> SpecialKey KeyEnd
        GLFW.Key'Pad0        -> SpecialKey KeyPad0
        GLFW.Key'Pad1        -> SpecialKey KeyPad1
        GLFW.Key'Pad2        -> SpecialKey KeyPad2
        GLFW.Key'Pad3        -> SpecialKey KeyPad3
        GLFW.Key'Pad4        -> SpecialKey KeyPad4
        GLFW.Key'Pad5        -> SpecialKey KeyPad5
        GLFW.Key'Pad6        -> SpecialKey KeyPad6
        GLFW.Key'Pad7        -> SpecialKey KeyPad7
        GLFW.Key'Pad8        -> SpecialKey KeyPad8
        GLFW.Key'Pad9        -> SpecialKey KeyPad9
        GLFW.Key'PadDivide   -> SpecialKey KeyPadDivide
        GLFW.Key'PadMultiply -> SpecialKey KeyPadMultiply
        GLFW.Key'PadSubtract -> SpecialKey KeyPadSubtract
        GLFW.Key'PadAdd      -> SpecialKey KeyPadAdd
        GLFW.Key'PadDecimal  -> SpecialKey KeyPadDecimal
        GLFW.Key'PadEqual    -> Char '='
        GLFW.Key'PadEnter    -> SpecialKey KeyPadEnter
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
        GLFW.MouseButton'1 -> MouseButton LeftButton
        GLFW.MouseButton'2 -> MouseButton RightButton
        GLFW.MouseButton'3 -> MouseButton MiddleButton
        GLFW.MouseButton'4 -> MouseButton $ AdditionalButton 4
        GLFW.MouseButton'5 -> MouseButton $ AdditionalButton 5
        GLFW.MouseButton'6 -> MouseButton $ AdditionalButton 6
        GLFW.MouseButton'7 -> MouseButton $ AdditionalButton 7
        GLFW.MouseButton'8 -> MouseButton $ AdditionalButton 8