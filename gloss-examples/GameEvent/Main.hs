
import Graphics.Gloss

-- | Display the last event received as text.
main
 = play (InWindow "GameEvent" (700, 100) (10, 10))
        white
        100
        ""
        (\str     -> Translate (-340) 0 $ Scale 0.1 0.1 $ Text str)
        (\event _ -> show event)
        (\_ world -> world)
        

