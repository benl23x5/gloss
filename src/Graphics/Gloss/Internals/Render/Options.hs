{-# OPTIONS_HADDOCK hide #-}

-- | Rendering options
module Graphics.Gloss.Internals.Render.Options 
	( Options (..)
	, optionsInit )
where

-- | Render options settings
data Options
	= Options
	{ -- | Whether to use color
	  optionsColor		:: Bool

	  -- | Whether to force wireframe mode only
	, optionsWireframe	:: Bool

	  -- | Whether to use alpha blending
	, optionsBlendAlpha	:: Bool

	  -- | Whether to use line smoothing
	, optionsLineSmooth	:: Bool
	}
	

-- | Default render options
optionsInit
	= Options
	{ optionsColor		= True
	, optionsWireframe	= False
	, optionsBlendAlpha	= True
	, optionsLineSmooth	= False }
	

