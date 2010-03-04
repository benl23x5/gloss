{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PatternGuards #-}

module Graphics.Gloss.Internals.Interface.ViewPort.Command
	( Command (..)
	, defaultCommandConfig
	, isCommand )
where
import Data.Map					(Map)
import qualified Graphics.UI.GLUT		as GLUT
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
		[ (GLUT.Char 'r', 			Nothing) ])

	, (CTranslate,
		[ ( GLUT.MouseButton GLUT.LeftButton
		  , Just (GLUT.Modifiers { GLUT.shift = GLUT.Up, GLUT.ctrl = GLUT.Up, GLUT.alt = GLUT.Up }))
		])
	
	, (CRotate,
		[ ( GLUT.MouseButton GLUT.RightButton
		  , Nothing)
		, ( GLUT.MouseButton GLUT.LeftButton
		  , Just (GLUT.Modifiers { GLUT.shift = GLUT.Up, GLUT.ctrl = GLUT.Down, GLUT.alt = GLUT.Up }))
	 	])
	
	-- bump zoom
	, (CBumpZoomOut,	
		[ (GLUT.MouseButton GLUT.WheelDown,	Nothing)
		, (GLUT.SpecialKey  GLUT.KeyPageDown,	Nothing) ])

	, (CBumpZoomIn,
		[ (GLUT.MouseButton GLUT.WheelUp, 	Nothing)
		, (GLUT.SpecialKey  GLUT.KeyPageUp,	Nothing)] )
	
	-- bump translate
	, (CBumpLeft,
		[ (GLUT.SpecialKey  GLUT.KeyLeft,	Nothing) ])

	, (CBumpRight,
		[ (GLUT.SpecialKey  GLUT.KeyRight,	Nothing) ])

	, (CBumpUp,
		[ (GLUT.SpecialKey  GLUT.KeyUp,		Nothing) ])

	, (CBumpDown,
		[ (GLUT.SpecialKey  GLUT.KeyDown,	Nothing) ])

	-- bump rotate
	, (CBumpClockwise,
		[ (GLUT.SpecialKey  GLUT.KeyHome,	Nothing) ])
	
	, (CBumpCClockwise,
		[ (GLUT.SpecialKey  GLUT.KeyEnd,	Nothing) ])

	]


isCommand commands c key keyMods
	| Just csMatch		<- Map.lookup c commands
	= or $ map (isCommand2 c key keyMods) csMatch 

	| otherwise
	= False

isCommand2 c key keyMods cMatch
	| (keyC, mModsC)	<- cMatch
	, keyC == key
	, case mModsC of
		Nothing		-> True
		Just modsC 	-> modsC == keyMods
	= True
	
	| otherwise
	= False
