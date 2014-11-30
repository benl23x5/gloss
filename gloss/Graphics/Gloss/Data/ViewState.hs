module Graphics.Gloss.Data.ViewState
        ( Command      (..)
        , CommandConfig
        , defaultCommandConfig
        , ViewState     (..)
        , viewStateInit
        , viewStateInitWithConfig
        , updateViewStateWithEvent
        , updateViewStateWithEventMaybe)
where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Event
import qualified Data.Map                       as Map
import Data.Map                                 (Map)
import Data.Maybe
import Control.Monad (mplus)


-- | The commands suported by the view controller.
data Command
        = CRestore

        | CTranslate
        | CRotate

        -- bump zoom
        | CBumpZoomOut
        | CBumpZoomIn

        -- bump translate
        | CBumpLeft
        | CBumpRight
        | CBumpUp
        | CBumpDown

        -- bump rotate
        | CBumpClockwise
        | CBumpCClockwise
        deriving (Show, Eq, Ord)


type CommandConfig = [(Command, [(Key, Maybe Modifiers)])]


-- | The default commands.  Left click pans, wheel zooms, right click
--   rotates, "r" key resets.
defaultCommandConfig :: CommandConfig
defaultCommandConfig
 =      [ (CRestore,
                [ (Char 'r',                    Nothing) ])

        , (CTranslate,
                [ ( MouseButton LeftButton
                  , Just (Modifiers { shift = Up, ctrl = Up, alt = Up }))
                ])

        , (CRotate,
                [ ( MouseButton RightButton
                  , Nothing)
                , ( MouseButton LeftButton
                  , Just (Modifiers { shift = Up, ctrl = Down, alt = Up }))
                ])

        -- bump zoom
        , (CBumpZoomOut,
                [ (MouseButton WheelDown,       Nothing)
                , (SpecialKey  KeyPageDown,     Nothing) ])

        , (CBumpZoomIn,
                [ (MouseButton WheelUp,         Nothing)
                , (SpecialKey  KeyPageUp,       Nothing)] )

        -- bump translate
        , (CBumpLeft,
                [ (SpecialKey  KeyLeft,         Nothing) ])

        , (CBumpRight,
                [ (SpecialKey  KeyRight,        Nothing) ])

        , (CBumpUp,
                [ (SpecialKey  KeyUp,           Nothing) ])

        , (CBumpDown,
                [ (SpecialKey  KeyDown,         Nothing) ])

        -- bump rotate
        , (CBumpClockwise,
                [ (SpecialKey  KeyHome,         Nothing) ])

        , (CBumpCClockwise,
                [ (SpecialKey  KeyEnd,          Nothing) ])

        ]


-- | Check if the provided key combination is some gloss viewport command.
isCommand 
        :: Map Command [(Key, Maybe Modifiers)] 
        -> Command -> Key -> Modifiers -> Bool

isCommand commands c key keyMods
        | Just csMatch          <- Map.lookup c commands
        = or $ map (isCommand2 c key keyMods) csMatch

        | otherwise
        = False


-- | Check if the provided key combination is some gloss viewport command.
isCommand2 :: Command -> Key -> Modifiers -> (Key, Maybe Modifiers) -> Bool
isCommand2 _ key keyMods cMatch
        | (keyC, mModsC)        <- cMatch
        , keyC == key
        , case mModsC of
                Nothing         -> True
                Just modsC      -> modsC == keyMods
        = True

        | otherwise
        = False


-- ViewControl State -----------------------------------------------------------
-- | State for controlling the viewport.
--      These are used by the viewport control component.
data ViewState
        = ViewState {
        -- | The command list for the viewport controller.
        --      These can be safely overwridden at any time by deleting
        --      or adding entries to the list.
        --      Entries at the front of the list take precedence.
          viewStateCommands             :: !(Map Command [(Key, Maybe Modifiers)])

        -- | How much to scale the world by for each step of the mouse wheel.
        , viewStateScaleStep            :: !Float

        -- | How many degrees to rotate the world by for each pixel of x motion.
        , viewStateRotateFactor         :: !Float

        -- | During viewport translation,
        --      where the mouse was clicked on the window.
        , viewStateTranslateMark        :: !(Maybe (Float, Float))

        -- | During viewport rotation,  
        --      where the mouse was clicked on the window
        , viewStateRotateMark           :: !(Maybe (Float, Float))

        , viewStateViewPort             :: ViewPort
        }


-- | The initial view state.
viewStateInit :: ViewState
viewStateInit
        = viewStateInitWithConfig defaultCommandConfig

-- | Initial view state, with user defined config.
viewStateInitWithConfig :: CommandConfig -> ViewState
viewStateInitWithConfig commandConfig
        = ViewState
        { viewStateCommands             = Map.fromList commandConfig
        , viewStateScaleStep            = 0.85
        , viewStateRotateFactor         = 0.6
        , viewStateTranslateMark        = Nothing
        , viewStateRotateMark           = Nothing
        , viewStateViewPort             = viewPortInit }


-- | Apply an event to a `ViewState`.
updateViewStateWithEvent :: Event -> ViewState -> ViewState
updateViewStateWithEvent ev viewState
        = fromMaybe viewState $ updateViewStateWithEventMaybe ev viewState


-- | Like 'updateViewStateWithEvent', but returns 'Nothing' if no update
--   was needed.
updateViewStateWithEventMaybe :: Event -> ViewState -> Maybe ViewState
updateViewStateWithEventMaybe (EventKey key keyState keyMods pos) viewState
        | isCommand commands CRestore key keyMods
        , keyState      == Down
        = Just $ viewState { viewStateViewPort = viewPortInit }

        | isCommand commands CBumpZoomOut key keyMods
        , keyState      == Down
        = Just $ controlZoomIn viewState

        | isCommand commands CBumpZoomIn key keyMods
        , keyState      == Down
        = Just $ controlZoomOut viewState

        | isCommand commands CBumpLeft key keyMods
        , keyState      == Down
        = Just $ viewState { viewStateViewPort = motionBump port (20, 0) }

        | isCommand commands CBumpRight key keyMods
        , keyState      == Down
        = Just $ viewState { viewStateViewPort = motionBump port (-20, 0) }

        | isCommand commands CBumpUp key keyMods
        , keyState      == Down
        = Just $ viewState { viewStateViewPort = motionBump port (0, -20) }

        | isCommand commands CBumpDown key keyMods
        , keyState      == Down
        = Just $ viewState { viewStateViewPort = motionBump port (0, 20) }

        | isCommand commands CBumpClockwise key keyMods
        , keyState      == Down
        = Just $ viewState { viewStateViewPort 
                                = port { viewPortRotate = viewPortRotate port + 5 } }

        | isCommand commands CBumpCClockwise key keyMods
        , keyState      == Down
        = Just $ viewState { viewStateViewPort 
                                = port { viewPortRotate = viewPortRotate port - 5 } }

        | isCommand commands CTranslate key keyMods
        , keyState      == Down
        , not currentlyRotating
        = Just $ viewState { viewStateTranslateMark = Just pos }

        -- We don't want to use 'isCommand' here because the user may have
        -- released the translation modifier key before the mouse button.
        -- and we still want to cancel the translation.
        | currentlyTranslating
        , keyState      == Up
        = Just $ viewState { viewStateTranslateMark = Nothing }

        | isCommand commands CRotate key keyMods
        , keyState      == Down
        , not currentlyTranslating
        = Just $ viewState { viewStateRotateMark = Just pos }

        -- We don't want to use 'isCommand' here because the user may have
        -- released the rotation modifier key before the mouse button, 
        -- and we still want to cancel the rotation.
        | currentlyRotating
        , keyState      == Up
        = Just $ viewState { viewStateRotateMark = Nothing }

        | otherwise
        = Nothing
        where   commands                = viewStateCommands viewState
                port                    = viewStateViewPort viewState
                currentlyTranslating    = isJust $ viewStateTranslateMark viewState
                currentlyRotating       = isJust $ viewStateRotateMark viewState


-- Note that only a translation or rotation applies, not both at the same time.
updateViewStateWithEventMaybe (EventMotion pos) viewState
 = motionTranslate (viewStateTranslateMark viewState) pos viewState `mplus`
   motionRotate    (viewStateRotateMark    viewState) pos viewState

updateViewStateWithEventMaybe (EventResize _) _ 
 = Nothing


-- | Zoom in a `ViewState` by the scale step.
controlZoomIn :: ViewState -> ViewState
controlZoomIn 
 viewState@ViewState 
        { viewStateViewPort     = port
        , viewStateScaleStep    = scaleStep }
 = viewState 
        { viewStateViewPort     
                = port { viewPortScale = viewPortScale port * scaleStep } }


-- | Zoom out a `ViewState` by the scale step.
controlZoomOut :: ViewState -> ViewState
controlZoomOut 
 viewState@ViewState 
        { viewStateViewPort     = port
        , viewStateScaleStep    = scaleStep }
 = viewState
        { viewStateViewPort     
                = port { viewPortScale = viewPortScale port / scaleStep } }


-- | Offset a viewport.
motionBump :: ViewPort -> (Float, Float) -> ViewPort
motionBump
        port@ViewPort   
        { viewPortTranslate     = trans
        , viewPortScale         = scale
        , viewPortRotate        = r }
        (bumpX, bumpY)
 = port { viewPortTranslate = trans - o }
 where  offset  = (bumpX / scale, bumpY / scale)
        o       = rotateV (degToRad r) offset


-- | Apply a translation to the `ViewState`.
motionTranslate 
        :: Maybe (Float, Float) 
        -> (Float, Float) 
        -> ViewState -> Maybe ViewState

motionTranslate Nothing _ _ = Nothing
motionTranslate (Just (markX, markY)) (posX, posY) viewState
 = Just $ viewState
        { viewStateViewPort      = port { viewPortTranslate = trans - o }
        , viewStateTranslateMark = Just (posX, posY) }

 where  port    = viewStateViewPort viewState
        trans   = viewPortTranslate port
        scale   = viewPortScale port
        r       = viewPortRotate port
        dX      = markX - posX
        dY      = markY - posY
        offset  = (dX / scale, dY / scale)
        o       = rotateV (degToRad r) offset


-- | Apply a rotation to the `ViewState`.
motionRotate 
        :: Maybe (Float, Float) 
        -> (Float, Float) 
        -> ViewState -> Maybe ViewState

motionRotate Nothing _ _ = Nothing
motionRotate (Just (markX, _markY)) (posX, posY) viewState
 = Just $ viewState
        { viewStateViewPort     
                = port { viewPortRotate = rotate - rotateFactor * (posX - markX) }

        , viewStateRotateMark   = Just (posX, posY) }
 where  port            = viewStateViewPort viewState
        rotate          = viewPortRotate port
        rotateFactor    = viewStateRotateFactor viewState

