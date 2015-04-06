
-- A fractal consisting of circles and lines which looks a bit like
--      the workings of a clock.
import Graphics.Gloss

main
 =      animate (InWindow "Clock" (600, 600) (20, 20))
                black frame


-- Build the fractal, scale it so it fits in the window
-- and rotate the whole thing as time moves on.
frame :: Float -> Picture
frame   time
        = Color white
        $ Scale 120 120
        $ Rotate (time * 2*pi)
        $ clockFractal 5 time
                

-- The basic fractal consists of three circles offset from the origin
-- as follows.
--
--         1
--         |
--         .
--       /   \
--      2     3
--
-- The direction of rotation switches as n increases.
-- Components at higher iterations also spin faster.
--
clockFractal :: Int -> Float -> Picture
clockFractal 0 s        = Blank
clockFractal n s        = Pictures [circ1, circ2, circ3, lines]
 where
        -- y offset from origin to center of circle 1.
        a       = 1 / sin (2 * pi / 6)

        -- x offset from origin to center of circles 2 and 3.
        b       = a * cos (2 * pi / 6)

        nf      = fromIntegral n
        rot     = if n `mod` 2 == 0
                        then   50 * s * (log (1 + nf))
                        else (-50 * s * (log (1 + nf)))

        -- each element contains a copy of the (n-1) iteration contained
        --      within a larger circle, and some text showing the time since 
        --      the animation started.
        --
        circNm1 
         = Pictures
                [ circle 1
                , Scale (a/2.5) (a/2.5) $ clockFractal (n-1) s
                , if n > 2
                    then Color cyan     
                                $ Translate (-0.15) 1
                                $ Scale 0.001 0.001 
                                $ Text (show s) 
                    else Blank
                ]

        circ1   = Translate 0 a         $ Rotate rot    circNm1
        circ2   = Translate 1 (-b)      $ Rotate (-rot) circNm1
        circ3   = Translate (-1) (-b)   $ Rotate rot    circNm1
        
        -- join each iteration to the origin with some lines.
        lines   
         = Pictures
                [ Line [(0, 0), ( 0,  a)]
                , Line [(0, 0), ( 1, -b)]
                , Line [(0, 0), (-1, -b)] ]
