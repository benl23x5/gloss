
{-# OPTIONS -fspec-constr-count=5 #-}
{-# LANGUAGE Rank2Types #-}
module Graphics.Gloss.Internals.Interface.Backend.Types
        ( module Graphics.Gloss.Internals.Interface.Backend.Types
        , module Graphics.Gloss.Data.Display)
where
import Data.IORef
import Graphics.Gloss.Data.Display


-- | The functions every backend window managed backend needs to support.
--
--   The Backend module interfaces with the window manager, and handles opening
--   and closing the window, and managing key events etc.
--
--   It doesn't know anything about drawing lines or setting colors.
--   When we get a display callback, Gloss will perform OpenGL actions, and
--   the backend needs to have OpenGL in a state where it's able to accept them.
--
class Backend a where
        -- | Initialize the state used by the backend. If you don't use any state,
        -- make a Unit-like type; see the GLUT backend for an example.
        initBackendState           :: a

        -- | Perform any initialization that needs to happen before opening a window
        --   The Boolean flag indicates if any debug information should be printed to
        --   the terminal
        initializeBackend          :: IORef a -> Bool -> IO ()

        -- | Perform any deinitialization and close the backend.
        exitBackend                :: IORef a -> IO ()

        -- | Open a window with the given display mode.
        openWindow                 :: IORef a -> Display -> IO ()

        -- | Dump information about the backend to the terminal.
        dumpBackendState           :: IORef a -> IO ()

        -- | Install the display callbacks.
        installDisplayCallback     :: IORef a -> [Callback] -> IO ()

        -- | Install the window close callback.
        installWindowCloseCallback :: IORef a -> IO ()

        -- | Install the reshape callbacks.
        installReshapeCallback     :: IORef a -> [Callback] -> IO ()

        -- | Install the keymouse press callbacks.
        installKeyMouseCallback    :: IORef a -> [Callback] -> IO ()

        -- | Install the mouse motion callbacks.
        installMotionCallback      :: IORef a -> [Callback] -> IO ()

        -- | Install the idle callbacks.
        installIdleCallback        :: IORef a -> [Callback] -> IO ()

        -- | The mainloop of the backend.
        runMainLoop                :: IORef a -> IO ()

        -- | A function that signals that screen has to be updated.
        postRedisplay              :: IORef a -> IO ()

        -- | Function that returns (width,height) of the window in pixels.
        getWindowDimensions        :: IORef a -> IO (Int,Int)

        -- | Function that reports the time elapsed since the application started.
        --   (in seconds)
        elapsedTime                :: IORef a -> IO Double

        -- | Function that puts the current thread to sleep for 'n' seconds.
        sleep                      :: IORef a -> Double -> IO ()


-- The callbacks should work for all backends. We pass a reference to the
-- backend state so that the callbacks have access to the class dictionary and
-- can thus call the appropriate backend functions.

-- | Display callback has no arguments.
type DisplayCallback
        = forall a . Backend a => IORef a -> IO ()

-- | Arguments: KeyType, Key Up \/ Down, Ctrl \/ Alt \/ Shift pressed, latest mouse location.
type KeyboardMouseCallback
        = forall a . Backend a => IORef a -> Key -> KeyState -> Modifiers -> (Int,Int) -> IO ()

-- | Arguments: (PosX,PosY) in pixels.
type MotionCallback
        = forall a . Backend a => IORef a -> (Int,Int) -> IO ()

-- | No arguments.
type IdleCallback
        = forall a . Backend a => IORef a -> IO ()

-- | Arguments: (Width,Height) in pixels.
type ReshapeCallback
        = forall a . Backend a => IORef a -> (Int,Int) -> IO ()


-------------------------------------------------------------------------------
data Callback
        = Display  DisplayCallback
        | KeyMouse KeyboardMouseCallback
        | Idle     IdleCallback
        | Motion   MotionCallback
        | Reshape  ReshapeCallback


-- | Check if this is an `Idle` callback.
isIdleCallback :: Callback -> Bool
isIdleCallback cc
 = case cc of
        Idle _  -> True
        _       -> False


-------------------------------------------------------------------------------
-- This is Glosses view of mouse and keyboard events.
-- The actual events provided by the backends are converted to this form
-- by the backend module.

data Key
        = Char        Char
        | SpecialKey  SpecialKey
        | MouseButton MouseButton
        deriving (Show, Eq, Ord)

data MouseButton
        = LeftButton
        | MiddleButton
        | RightButton
        | WheelUp
        | WheelDown
        | AdditionalButton Int
        deriving (Show, Eq, Ord)

data KeyState
        = Down
        | Up
        deriving (Show, Eq, Ord)

data SpecialKey
        = KeyUnknown
        | KeySpace
        | KeyEsc
        | KeyF1
        | KeyF2
        | KeyF3
        | KeyF4
        | KeyF5
        | KeyF6
        | KeyF7
        | KeyF8
        | KeyF9
        | KeyF10
        | KeyF11
        | KeyF12
        | KeyF13
        | KeyF14
        | KeyF15
        | KeyF16
        | KeyF17
        | KeyF18
        | KeyF19
        | KeyF20
        | KeyF21
        | KeyF22
        | KeyF23
        | KeyF24
        | KeyF25
        | KeyUp
        | KeyDown
        | KeyLeft
        | KeyRight
        | KeyTab
        | KeyEnter
        | KeyBackspace
        | KeyInsert
        | KeyNumLock
        | KeyBegin
        | KeyDelete
        | KeyPageUp
        | KeyPageDown
        | KeyHome
        | KeyEnd
        | KeyShiftL
        | KeyShiftR
        | KeyCtrlL
        | KeyCtrlR
        | KeyAltL
        | KeyAltR
        | KeyPad0
        | KeyPad1
        | KeyPad2
        | KeyPad3
        | KeyPad4
        | KeyPad5
        | KeyPad6
        | KeyPad7
        | KeyPad8
        | KeyPad9
        | KeyPadDivide
        | KeyPadMultiply
        | KeyPadSubtract
        | KeyPadAdd
        | KeyPadDecimal
        | KeyPadEqual
        | KeyPadEnter
        deriving (Show, Eq, Ord)


data Modifiers
        = Modifiers
        { shift :: KeyState
        , ctrl  :: KeyState
        , alt   :: KeyState }
        deriving (Show, Eq, Ord)

