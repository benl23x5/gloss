{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Interface.ViewPort.ControlState
	( State (..)
	, stateInit )
where
import Graphics.Gloss.Internals.Interface.ViewPort.Command
import Graphics.Gloss.Internals.Interface.Backend
import qualified Data.Map			as Map
import Data.Map					(Map)

-- ViewControl State ------------------------------------------------------------------------------
-- | State for controlling the viewport.
--	These are used by the viewport control component.
data State
	= State {
	-- | The command list for the viewport controller.
	--	These can be safely overwridden at any time by deleting / adding entries to the list.
	--	Entries at the front of the list take precedence.
	  stateCommands		:: Map Command [(Key, Maybe Modifiers)]

	-- | How much to scale the world by for each step of the mouse wheel.
	, stateScaleStep	:: Float	
							
	-- | How many degrees to rotate the world by for each pixel of x motion.
	, stateRotateFactor	:: Float

	-- | During viewport translation,
	--	where the mouse was clicked on the window.
	, stateTranslateMark	:: Maybe (Int, Int)	

	-- | During viewport rotation,	
	--	where the mouse was clicked on the window
	, stateRotateMark	:: Maybe (Int, Int)
	}


-- | The initial view state.
stateInit :: State
stateInit
	= State
	{ stateCommands		= Map.fromList defaultCommandConfig
	, stateScaleStep	= 0.85
	, stateRotateFactor	= 0.6
	, stateTranslateMark	= Nothing
	, stateRotateMark	= Nothing }
