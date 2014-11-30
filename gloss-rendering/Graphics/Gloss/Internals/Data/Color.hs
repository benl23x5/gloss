{-# OPTIONS_HADDOCK hide #-}

-- | Data type for representing colors.
module Graphics.Gloss.Internals.Data.Color
        ( Color (..)
        , makeColor
        , makeColor8
        , rgbaOfColor
        , makeRawColor)
where
import Data.Data

-- | An abstract color value.
--      We keep the type abstract so we can be sure that the components
--      are in the required range. To make a custom color use 'makeColor'.
data Color
        -- | Holds the color components. All components lie in the range [0..1.
        = RGBA  !Float !Float !Float !Float
        deriving (Show, Eq, Data, Typeable)


instance Num Color where
 (+) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 + r2) (g1 + g2) (b1 + b2) 1
 {-# INLINE (+) #-}

 (-) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 - r2) (g1 - g2) (b1 - b2) 1
 {-# INLINE (-) #-}

 (*) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 * r2) (g1 * g2) (b1 * b2) 1
 {-# INLINE (*) #-}

 abs (RGBA r1 g1 b1 _)
        = RGBA (abs r1) (abs g1) (abs b1) 1
 {-# INLINE abs #-}

 signum (RGBA r1 g1 b1 _)
        = RGBA (signum r1) (signum g1) (signum b1) 1
 {-# INLINE signum #-}
        
 fromInteger i
  = let f = fromInteger i
    in  RGBA f f f 1
 {-# INLINE fromInteger #-}


-- | Make a custom color. All components are clamped to the range  [0..1].
makeColor 
        :: Float        -- ^ Red component.
        -> Float        -- ^ Green component.
        -> Float        -- ^ Blue component.
        -> Float        -- ^ Alpha component.
        -> Color

makeColor r g b a
        = clampColor 
        $ RGBA r g b a
{-# INLINE makeColor #-}


-- | Make a custom color. All components are clamped to the range [0..255].
makeColor8 
        :: Int          -- ^ Red component.
        -> Int          -- ^ Green component.
        -> Int          -- ^ Blue component.
        -> Int          -- ^ Alpha component.
        -> Color

makeColor8 r g b a
        = clampColor 
        $ RGBA  (fromIntegral r / 255) 
                (fromIntegral g / 255)
                (fromIntegral b / 255)
                (fromIntegral a / 255)
{-# INLINE makeColor8 #-}


-- | Take the RGBA components of a color.
rgbaOfColor :: Color -> (Float, Float, Float, Float)
rgbaOfColor (RGBA r g b a)      = (r, g, b, a)
{-# INLINE rgbaOfColor #-}
                


-- Internal 
-- | Clamp components of a color into the required range.
clampColor :: Color -> Color
clampColor cc
 = let  (r, g, b, a)    = rgbaOfColor cc
   in   RGBA (min 1 r) (min 1 g) (min 1 b) (min 1 a)


-- | Make a custom color. 
--
--   Using this function over `makeColor` avoids clamping the components,
--   which saves time. However, if the components are out of range then
--   this will result in integer overflow at rendering time, and the actual
--   picture you get will be implementation dependent.
--   
makeRawColor :: Float -> Float -> Float -> Float -> Color
makeRawColor r g b a
        = RGBA r g b a
{-# INLINE makeRawColor #-}

        
