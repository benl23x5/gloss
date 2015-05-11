
-- Draw a color wheel.
import Graphics.Gloss

main 
 = display 
        (InWindow  "Colors" (800, 800) (5, 5))
        (greyN 0.4)
        (Pictures 
                [ Translate 
                        (200 * cos (2 * pi * (fromIntegral n) / 12))
                        (200 * sin (2 * pi * (fromIntegral n) / 12))
                $ Color (withAlpha 0.8 c) $ circleSolid 100
                        | n <- [0 .. length colors]
                        | c <- colors ])
colors
 =      [ red
        , orange
        , yellow
        , chartreuse
        , green
        , aquamarine
        , cyan
        , azure
        , blue
        , violet
        , magenta
        , rose
        ]


