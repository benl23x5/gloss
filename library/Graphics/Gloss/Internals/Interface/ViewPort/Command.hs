{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE PatternGuards #-}

module Graphics.Gloss.Internals.Interface.ViewPort.Command
	( Command (..)
	, defaultCommandConfig
	, isCommand )
where
import Graphics.Gloss.Internals.Interface.Backend
import qualified Data.Map			as Map

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
