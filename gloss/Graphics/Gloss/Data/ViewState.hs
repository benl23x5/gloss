{-# LANGUAGE PatternGuards #-}
module Graphics.Gloss.Data.ViewState
	( ViewState (..)
	, viewStateInit
        , updateViewStateWithEvent
        , applyViewPortToPicture )
where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Picture (Picture(..))
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Game
import Graphics.Gloss.Internals.Interface.ViewPort
import qualified Data.Map			as Map
import Data.Map					(Map)
import Data.Maybe

-- | The commands suported by the view controller
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


-- | The default commands
defaultCommandConfig :: [(Command, [(Key, Maybe Modifiers)])]
defaultCommandConfig
 =	[ (CRestore, 	
		[ (Char 'r', 			Nothing) ])

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
		[ (MouseButton WheelDown,	Nothing)
		, (SpecialKey  KeyPageDown,	Nothing) ])

	, (CBumpZoomIn,
		[ (MouseButton WheelUp, 	Nothing)
		, (SpecialKey  KeyPageUp,	Nothing)] )
	
	-- bump translate
	, (CBumpLeft,
		[ (SpecialKey  KeyLeft,	        Nothing) ])

	, (CBumpRight,
		[ (SpecialKey  KeyRight,	Nothing) ])

	, (CBumpUp,
		[ (SpecialKey  KeyUp,		Nothing) ])

	, (CBumpDown,
		[ (SpecialKey  KeyDown,	        Nothing) ])

	-- bump rotate
	, (CBumpClockwise,
		[ (SpecialKey  KeyHome,	        Nothing) ])
	
	, (CBumpCClockwise,
		[ (SpecialKey  KeyEnd,	        Nothing) ])

	]


isCommand commands c key keyMods
	| Just csMatch		<- Map.lookup c commands
	= or $ map (isCommand2 c key keyMods) csMatch 

	| otherwise
	= False

isCommand2 _ key keyMods cMatch
	| (keyC, mModsC)	<- cMatch
	, keyC == key
	, case mModsC of
		Nothing		-> True
		Just modsC 	-> modsC == keyMods
	= True
	
	| otherwise
	= False

-- ViewControl State ------------------------------------------------------------------------------
-- | State for controlling the viewport.
--	These are used by the viewport control component.
data ViewState
	= ViewState {
	-- | The command list for the viewport controller.
	--	These can be safely overwridden at any time by deleting / adding entries to the list.
	--	Entries at the front of the list take precedence.
	  viewStateCommands		:: !(Map Command [(Key, Maybe Modifiers)])

	-- | How much to scale the world by for each step of the mouse wheel.
	, viewStateScaleStep		:: !Float	
							
	-- | How many degrees to rotate the world by for each pixel of x motion.
	, viewStateRotateFactor		:: !Float

	-- | During viewport translation,
	--	where the mouse was clicked on the window.
	, viewStateTranslateMark	:: !(Maybe (Float, Float))

	-- | During viewport rotation,	
	--	where the mouse was clicked on the window
	, viewStateRotateMark		:: !(Maybe (Float, Float))

        , viewStateViewPort		:: ViewPort
	}


-- | The initial view state.
viewStateInit :: ViewPort -> ViewState
viewStateInit viewPort
	= ViewState
	{ viewStateCommands		= Map.fromList defaultCommandConfig
	, viewStateScaleStep		= 0.85
	, viewStateRotateFactor		= 0.6
	, viewStateTranslateMark	= Nothing
	, viewStateRotateMark		= Nothing
        , viewStateViewPort 		= viewPort }

-- TODO use record syntax instead of projections
updateViewStateWithEvent :: Event -> ViewState -> ViewState
updateViewStateWithEvent (EventKey key keyState keyMods pos) viewState
	| isCommand commands CRestore key keyMods
	, keyState	== Down
	= viewState { viewStateViewPort = viewPortInit }
	| isCommand commands CBumpZoomOut key keyMods
	, keyState	== Down
	= controlZoomIn viewState
	| isCommand commands CBumpZoomIn key keyMods
	, keyState	== Down
	= controlZoomOut viewState
	| isCommand commands CBumpLeft key keyMods
	, keyState	== Down
	= viewState { viewStateViewPort = motionBump port (20, 0) }
	| isCommand commands CBumpRight key keyMods
	, keyState	== Down
	= viewState { viewStateViewPort = motionBump port (-20, 0) }
	| isCommand commands CBumpUp key keyMods
	, keyState	== Down
	= viewState { viewStateViewPort = motionBump port (0, 20) }
	| isCommand commands CBumpDown key keyMods
	, keyState	== Down
	= viewState { viewStateViewPort = motionBump port (0, -20) }
	| isCommand commands CBumpClockwise key keyMods
	, keyState	== Down
	= viewState { viewStateViewPort = port { viewPortRotate = viewPortRotate port + 5 } }
	| isCommand commands CBumpCClockwise key keyMods
	, keyState	== Down
	= viewState { viewStateViewPort = port { viewPortRotate = viewPortRotate port - 5 } }
	| isCommand commands CTranslate key keyMods
	, keyState	== Down
	, not currentlyRotating
	= viewState { viewStateTranslateMark = Just pos }
	-- We don't want to use 'isCommand' here because the user may have
	-- released the translation modifier key before the mouse button.
	-- and we still want to cancel the translation.
	| currentlyTranslating
	, keyState	== Up
	= viewState { viewStateTranslateMark = Nothing }
	| isCommand commands CRotate key keyMods
	, keyState	== Down
	, not currentlyTranslating
	= viewState { viewStateRotateMark = Just pos }
	-- We don't want to use 'isCommand' here because the user may have
	-- released the rotation modifier key before the mouse button, 
	-- and we still want to cancel the rotation.
	| currentlyRotating
	, keyState	== Up
	= viewState { viewStateRotateMark = Nothing }
	| otherwise
	= viewState
	where	commands		= viewStateCommands viewState
		port			= viewStateViewPort viewState
		currentlyTranslating	= isJust $ viewStateTranslateMark viewState
                currentlyRotating	= isJust $ viewStateRotateMark viewState
updateViewStateWithEvent (EventMotion mov) viewState
	= undefined

controlZoomIn :: ViewState -> ViewState
controlZoomIn viewState@ViewState { viewStateViewPort = port, viewStateScaleStep = scaleStep }
	= viewState { viewStateViewPort = port { viewPortScale = viewPortScale port * scaleStep } }

controlZoomOut :: ViewState -> ViewState
controlZoomOut viewState@ViewState { viewStateViewPort = port, viewStateScaleStep = scaleStep }
	= viewState { viewStateViewPort = port { viewPortScale = viewPortScale port / scaleStep } }

motionBump :: ViewPort -> (Float, Float) -> ViewPort
motionBump
	port@ViewPort	{ viewPortTranslate	= (transX, transY)
                        , viewPortScale		= scale
			, viewPortRotate	= r }
	(bumpX, bumpY)
	= port { viewPortTranslate = (transX - oX, transY + oY) }
	where	offset		= (bumpX / scale, bumpY / scale)
		(oX, oY)	= rotateV (degToRad r) offset

applyViewPortToPicture :: ViewPort  -> Picture -> Picture
applyViewPortToPicture port
	= Rotate rotate . Translate transX transY . Scale scale scale
	where	rotate	= realToFrac $ viewPortRotate port
        	transX	= realToFrac $ fst $ viewPortTranslate port
        	transY	= realToFrac $ snd $ viewPortTranslate port
        	scale	= realToFrac $ viewPortScale port
