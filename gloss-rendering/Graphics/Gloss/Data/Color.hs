
-- | Predefined and custom colors.
module Graphics.Gloss.Data.Color
	( 
	-- ** Color data type
	  Color
	, makeColor
        , makeColor'
        , makeColor8
	, rawColor
	, rgbaOfColor

	-- ** Color functions
	, mixColors
	, addColors
	, dim,   bright
	, light, dark

	-- ** Pre-defined colors
	, greyN,  black,  white
	-- *** Primary
	, red,    green,  blue
	-- *** Secondary
	, yellow,     cyan,       magenta
	
	-- *** Tertiary
	, rose,   violet, azure, aquamarine, chartreuse, orange
	)
where
import Data.Data


-- | An abstract color value.
--	We keep the type abstract so we can be sure that the components
--	are in the required range. To make a custom color use 'makeColor'.
data Color
	-- | Holds the color components. All components lie in the range [0..1.
	= RGBA  !Float !Float !Float !Float
	deriving (Show, Eq, Data, Typeable)


instance Num Color where
 {-# INLINE (+) #-}
 (+) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 + r2) (g1 + g2) (b1 + b2) 1

 {-# INLINE (-) #-}
 (-) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 - r2) (g1 - g2) (b1 - b2) 1

 {-# INLINE (*) #-}
 (*) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 * r2) (g1 * g2) (b1 * b2) 1

 {-# INLINE abs #-}
 abs (RGBA r1 g1 b1 _)
        = RGBA (abs r1) (abs g1) (abs b1) 1

 {-# INLINE signum #-}
 signum (RGBA r1 g1 b1 _)
        = RGBA (signum r1) (signum g1) (signum b1) 1
        
 {-# INLINE fromInteger #-}
 fromInteger i
  = let f = fromInteger i
    in  RGBA f f f 1


-- | Make a custom color. All components are clamped to the range  [0..1].
makeColor 
	:: Float 	-- ^ Red component.
	-> Float 	-- ^ Green component.
	-> Float 	-- ^ Blue component.
	-> Float 	-- ^ Alpha component.
	-> Color

makeColor r g b a
	= clampColor 
	$ RGBA r g b a
{-# INLINE makeColor #-}


-- | Make a custom color. 
--   You promise that all components are clamped to the range [0..1]
makeColor' :: Float -> Float -> Float -> Float -> Color
makeColor' r g b a
        = RGBA r g b a
{-# INLINE makeColor' #-}


-- | Make a custom color. All components are clamped to the range [0..255].
makeColor8 
	:: Int 		-- ^ Red component.
	-> Int 		-- ^ Green component.
	-> Int 		-- ^ Blue component.
	-> Int 		-- ^ Alpha component.
	-> Color

makeColor8 r g b a
	= clampColor 
	$ RGBA 	(fromIntegral r / 255) 
		(fromIntegral g / 255)
		(fromIntegral b / 255)
		(fromIntegral a / 255)
{-# INLINE makeColor8 #-}

	
-- | Take the RGBA components of a color.
rgbaOfColor :: Color -> (Float, Float, Float, Float)
rgbaOfColor (RGBA r g b a)	= (r, g, b, a)
{-# INLINE rgbaOfColor #-}
		

-- | Make a custom color.
--   Components should be in the range [0..1] but this is not checked.
rawColor
	:: Float	-- ^ Red component.
	-> Float	-- ^ Green component.
	-> Float 	-- ^ Blue component.
	-> Float 	-- ^ Alpha component.
	-> Color

rawColor = RGBA
{-# INLINE rawColor #-}


-- Internal 

-- | Clamp components of a color into the required range.
clampColor :: Color -> Color
clampColor cc
 = let	(r, g, b, a)	= rgbaOfColor cc
   in	RGBA (min 1 r) (min 1 g) (min 1 b) (min 1 a)

-- | Normalise a color to the value of its largest RGB component.
normaliseColor :: Color -> Color
normaliseColor cc
 = let	(r, g, b, a)	= rgbaOfColor cc
	m		= maximum [r, g, b]
   in	RGBA (r / m) (g / m) (b / m) a


-- Color functions ------------------------------------------------------------

-- | Mix two colors with the given ratios.
mixColors 
	:: Float 	-- ^ Ratio of first color.
	-> Float 	-- ^ Ratio of second color.
	-> Color 	-- ^ First color.
	-> Color 	-- ^ Second color.
	-> Color	-- ^ Resulting color.

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


-- | Add RGB components of a color component-wise, then normalise
--	them to the highest resulting one. The alpha components are averaged.
addColors :: Color -> Color -> Color
addColors c1 c2
 = let	RGBA r1 g1 b1 a1	= c1
	RGBA r2 g2 b2 a2	= c2

   in	normaliseColor 
	 $ RGBA (r1 + r2)
		(g1 + g2)
		(b1 + b2)
		((a1 + a2) / 2)


-- | Make a dimmer version of a color, scaling towards black.
dim :: Color -> Color
dim (RGBA r g b a)
	= RGBA (r / 1.2) (g / 1.2) (b / 1.2) a

	
-- | Make a brighter version of a color, scaling towards white.
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
-- | A greyness of a given magnitude.
greyN 	:: Float 	-- ^ Range is 0 = black, to 1 = white.
	-> Color
greyN n		= RGBA n   n   n   1.0

black, white :: Color
black		= RGBA 0.0 0.0 0.0 1.0
white		= RGBA 1.0 1.0 1.0 1.0

-- Colors from the additive color wheel.
red, green, blue :: Color
red		= RGBA 1.0 0.0 0.0 1.0
green		= RGBA 0.0 1.0 0.0 1.0
blue		= RGBA 0.0 0.0 1.0 1.0

-- secondary
yellow, cyan, magenta :: Color
yellow		= addColors red   green
cyan		= addColors green blue
magenta		= addColors red   blue

-- tertiary
rose, violet, azure, aquamarine, chartreuse, orange :: Color
rose		= addColors red     magenta
violet		= addColors magenta blue
azure		= addColors blue    cyan
aquamarine	= addColors cyan    green
chartreuse	= addColors green   yellow
orange		= addColors yellow  red
