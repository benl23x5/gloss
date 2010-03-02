
-- | Colors.
module Graphics.Gloss.Color
	( Color
	, makeColor
	, makeColor8
	, takeRGBAOfColor
	, glColor4OfColor 

	-- Color functions
	, mixColors
	, addColors
	, dim,   bright
	, light, dark

	-- Pre-defined Colors
	, white,  black,  greyN 
	, red,    green,  blue,  yellow,     cyan,       magenta
	, rose,   violet, azure, aquamarine, chartreuse, orange
	)
where

import qualified Graphics.Rendering.OpenGL.GL	as GL

-- | An RGBA color.
--	We keep this type abstract so we can be sure the components
--	are in the correct range. If you want to make a custom color
--	then use makeColor. 
data Color
	-- | A color with floating point components.
	--	All components lie in the range [0..1]
	= RGBA  Float Float Float Float
	deriving (Show, Eq)


-- | Make a custom color using float components
--	in the range [0..1]
makeColor :: Float -> Float -> Float -> Float -> Color
makeColor r g b a
	= clampColor 
	$ RGBA r g b a


-- | Make a custom color using 8 bit integer components
--	in the range [0..255]
makeColor8 :: Int -> Int -> Int -> Int -> Color
makeColor8 r g b a
	= clampColor 
	$ RGBA 	(fromIntegral r / 255) 
		(fromIntegral g / 255)
		(fromIntegral b / 255)
		(fromIntegral a / 255)

	
-- | Take the RGBA components of a color, 
--	converting from RGBA if required.
takeRGBAOfColor :: Color -> (Float, Float, Float, Float)
takeRGBAOfColor (RGBA r g b a)	= (r, g, b, a)
		

-- | Convert one of our Color's to OpenGL's representation.
glColor4OfColor :: Fractional a => Color -> GL.Color4 a
glColor4OfColor (RGBA r g b a)
 = let	rF	= realToFrac r
	gF	= realToFrac g
	bF	= realToFrac b
	aF	= realToFrac a
   in	GL.Color4 rF gF bF aF


-- Internal 

-- | Clamp components of a color into the required range.
clampColor :: Color -> Color
clampColor cc
 = let	(r, g, b, a)	= takeRGBAOfColor cc
   in	RGBA (min 1 r) (min 1 g) (min 1 b) (min 1 a)

-- | Normalise a color to the value of its largest RGB component.
normaliseColor :: Color -> Color
normaliseColor cc
 = let	(r, g, b, a)	= takeRGBAOfColor cc
	m		= maximum [r, g, b]
   in	RGBA (r / m) (g / m) (b / m) a


-- Color functions ------------------------------------------------------------

-- Mix two colors with the given ratios.
mixColors :: Float -> Float -> Color -> Color -> Color
mixColors ratio1 ratio2 c1 c2
 = let	RGBA r1 g1 b1 a1	= c1
	RGBA r2 g2 b2 a2	= c2

	total	= ratio1 + ratio2
	m1	= ratio1 / total
	m2	= ratio2 / total

   in	RGBA 	(m1 * r1 + m2 * r2)
		(m1 * g1 + m2 * g2)
		(m1 * b1 + m2 * b2)
		(m1 * a1 + m2 * a2)


-- | Add the components of two colors, 
--	normalising their components.
addColors :: Color -> Color -> Color
addColors c1 c2
 = let	RGBA r1 g1 b1 a1	= c1
	RGBA r2 g2 b2 a2	= c2

   in	normaliseColor 
	 $ RGBA (r1 + r2)
		(g1 + g2)
		(b1 + b2)
		((a1 + a2) / 2)


-- | Create a dimmer version of a color, scaling towards black.
dim :: Color -> Color
dim (RGBA r g b a)
	= RGBA (r / 1.2) (g / 1.2) (b / 1.2) a

	
-- | Create a brighter version of a color, scaling towards white.
bright :: Color -> Color
bright (RGBA r g b a)
	= clampColor
	$ RGBA (r * 1.2) (g * 1.2) (b * 1.2) a


-- | Lighten a color, adding white.
light :: Color -> Color
light (RGBA r g b a)
	= clampColor
	$ RGBA (r + 0.2) (g + 0.2) (b + 0.2) a
	
	
-- | Darken a color, adding black.
dark :: Color -> Color
dark (RGBA r g b a)
	= clampColor
	$ RGBA (r - 0.2) (g - 0.2) (b - 0.2) a


-- Pre-defined Colors ---------------------------------------------------------

white		= RGBA 1.0 1.0 1.0 1.0
black		= RGBA 0.0 0.0 0.0 1.0
greyN n		= RGBA n   n   n   1.0

-- Colors from the additive color wheel.
-- primary
red		= RGBA 1.0 0.0 0.0 1.0
green		= RGBA 0.0 1.0 0.0 1.0
blue		= RGBA 0.0 0.0 1.0 1.0

-- secondary
yellow		= addColors red   green
cyan		= addColors green blue
magenta		= addColors red   blue

-- tertiary
rose		= addColors red     magenta
violet		= addColors magenta blue
azure		= addColors blue    cyan
aquamarine	= addColors cyan    green
chartreuse	= addColors green   yellow
orange		= addColors yellow  red
