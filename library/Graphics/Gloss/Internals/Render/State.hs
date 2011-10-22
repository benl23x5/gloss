{-# OPTIONS_HADDOCK hide #-}

-- | Rendering options
module Graphics.Gloss.Internals.Render.State
	( State (..)
	, stateInit )
where

-- | Render options settings
data State
	= State
	{ -- | Whether to use color
	  stateColor		:: Bool

	  -- | Whether to force wireframe mode only
	, stateWireframe	:: Bool

	  -- | Whether to use alpha blending
	, stateBlendAlpha	:: Bool

	  -- | Whether to use line smoothing
	, stateLineSmooth	:: Bool
	}
	

-- | Default render options
stateInit :: State
stateInit
	= State
	{ stateColor		= True
	, stateWireframe	= False
	, stateBlendAlpha	= True
	, stateLineSmooth	= False }
	

