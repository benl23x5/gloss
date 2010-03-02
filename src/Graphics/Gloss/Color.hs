
-- | Colors.
module Graphics.Gloss.Color
	(Color(..))
where


-- | An RGBA color.
data Color
	-- | A color with floating point components.
	--	All components lie in the range [0..1]
	= RGBA  Float Float Float Float
	
	-- | A color with 8 bit integer components.
	--	All components lie in the range [0..255]
	| RGBA8 Int   Int   Int   Int

	deriving (Show, Eq)
