{-# LANGUAGE PatternGuards #-}
module Graphics.Gloss.Data.ViewState
	( ViewState (..)
	, viewStateInit
        , updateViewStateWithEvents
        , applyViewPortToPicture )
where

import Graphics.Gloss.Data.Picture (Picture(..))
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Game
import Graphics.Gloss.Internals.Interface.ViewPort
import qualified Data.Map			as Map
import Data.Map					(Map)

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
	, viewStateTranslateMark	:: !(Maybe (Int, Int))

	-- | During viewport rotation,	
	--	where the mouse was clicked on the window
	, viewStateRotateMark		:: !(Maybe (Int, Int))

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

updateViewStateWithEvents :: [Event] -> ViewState -> ViewState
updateViewStateWithEvents events viewState
	= undefined

applyViewPortToPicture :: ViewPort  -> Picture -> Picture
applyViewPortToPicture port
	= Rotate rotate . Translate transX transY . Scale scale scale
	where	rotate	= realToFrac $ viewPortRotate port
        	transX	= realToFrac $ fst $ viewPortTranslate port
        	transY	= realToFrac $ snd $ viewPortTranslate port
        	scale	= realToFrac $ viewPortScale port

