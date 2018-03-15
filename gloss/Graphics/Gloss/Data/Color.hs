
-- | Predefined and custom colors.
module Graphics.Gloss.Data.Color
        ( -- ** Color data type
          Color
        , makeColor
        , makeColorI
        , rgbaOfColor

          -- ** Color functions
        , mixColors
        , addColors
        , dim,   bright
        , light, dark

        , withRed
        , withGreen
        , withBlue
        , withAlpha

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
import Graphics.Gloss.Rendering



-- Color functions ------------------------------------------------------------
-- | Mix two colors with the given ratios.
mixColors
        :: Float        -- ^ Proportion of first color.
        -> Float        -- ^ Proportion of second color.
        -> Color        -- ^ First color.
        -> Color        -- ^ Second color.
        -> Color        -- ^ Resulting color.

mixColors m1 m2 c1 c2
 = let  (r1, g1, b1, a1) = rgbaOfColor c1
        (r2, g2, b2, a2) = rgbaOfColor c2

        -- Normalise mixing proportions to ratios.
        m12 = m1 + m2
        m1' = m1 / m12
        m2' = m2 / m12

        -- Colors components should be added via sum of squares,
        -- otherwise the result will be too dark.
        r1s = r1 * r1;    r2s = r2 * r2
        g1s = g1 * g1;    g2s = g2 * g2
        b1s = b1 * b1;    b2s = b2 * b2

   in   makeColor
                (sqrt (m1' * r1s + m2' * r2s))
                (sqrt (m1' * g1s + m2' * g2s))
                (sqrt (m1' * b1s + m2' * b2s))
                ((m1 * a1   + m2 * a2) / m12)


-- | Add RGB components of a color component-wise,
--   then normalise them to the highest resulting one.
--   The alpha components are averaged.
addColors :: Color -> Color -> Color
addColors c1 c2
 = let  (r1, g1, b1, a1) = rgbaOfColor c1
        (r2, g2, b2, a2) = rgbaOfColor c2

   in   normalizeColor
                (r1 + r2)
                (g1 + g2)
                (b1 + b2)
                ((a1 + a2) / 2)


-- | Make a dimmer version of a color, scaling towards black.
dim :: Color -> Color
dim c
 = let  (r, g, b, a)    = rgbaOfColor c
   in   makeColor (r / 1.2) (g / 1.2) (b / 1.2) a


-- | Make a brighter version of a color, scaling towards white.
bright :: Color -> Color
bright c
 = let  (r, g, b, a)    = rgbaOfColor c
   in   makeColor (r * 1.2) (g * 1.2) (b * 1.2) a


-- | Lighten a color, adding white.
light :: Color -> Color
light c
 = let  (r, g, b, a)    = rgbaOfColor c
   in   makeColor (r + 0.2) (g + 0.2) (b + 0.2) a


-- | Darken a color, adding black.
dark :: Color -> Color
dark c
 = let  (r, g, b, a)    = rgbaOfColor c
   in   makeColor (r - 0.2) (g - 0.2) (b - 0.2) a


-------------------------------------------------------------------------------
-- | Set the red value of a `Color`.
withRed :: Float -> Color -> Color
withRed r c
 = let  (_, g, b, a) = rgbaOfColor c
   in   makeColor r g b a


-- | Set the green value of a `Color`.
withGreen :: Float -> Color -> Color
withGreen g c
 = let  (r, _, b, a) = rgbaOfColor c
   in   makeColor r g b a


-- | Set the blue value of a `Color`.
withBlue :: Float -> Color -> Color
withBlue b c
 = let  (r, g, _, a) = rgbaOfColor c
   in   makeColor r g b a


-- | Set the alpha value of a `Color`.
withAlpha :: Float -> Color -> Color
withAlpha a c
 = let  (r, g, b, _) = rgbaOfColor c
   in   makeColor r g b a


-- Pre-defined Colors ---------------------------------------------------------
-- | A greyness of a given order.
--
--   Range is 0 = black, to 1 = white.
greyN   :: Float -> Color
greyN n         = makeRawColor n   n   n   1.0

black, white :: Color
black           = makeRawColor 0.0 0.0 0.0 1.0
white           = makeRawColor 1.0 1.0 1.0 1.0

-- Colors from the additive color wheel.
red, green, blue :: Color
red             = makeRawColor 1.0 0.0 0.0 1.0
green           = makeRawColor 0.0 1.0 0.0 1.0
blue            = makeRawColor 0.0 0.0 1.0 1.0

-- secondary
yellow, cyan, magenta :: Color
yellow          = addColors red   green
cyan            = addColors green blue
magenta         = addColors red   blue

-- tertiary
rose, violet, azure, aquamarine, chartreuse, orange :: Color
rose            = addColors red     magenta
violet          = addColors magenta blue
azure           = addColors blue    cyan
aquamarine      = addColors cyan    green
chartreuse      = addColors green   yellow
orange          = addColors yellow  red


-------------------------------------------------------------------------------
-- | Normalise a color to the value of its largest RGB component.
normalizeColor :: Float -> Float -> Float -> Float -> Color
normalizeColor r g b a
 = let  m               = maximum [r, g, b]
   in   makeColor (r / m) (g / m) (b / m) a

