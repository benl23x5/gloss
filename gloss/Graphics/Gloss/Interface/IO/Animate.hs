
-- | Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.IO.Animate
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , animateIO)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Animate
import Graphics.Gloss.Internals.Interface.Backend


-- | Open a new window and display the given animation.
--
--   Once the window is open you can use the same commands as with @display@.
--
animateIO :: Display              -- ^ Display mode.
        -> Color                  -- ^ Background color.
        -> (Float -> IO Picture)  -- ^ Function to produce the next frame of animation. 
                                  --      It is passed the time in seconds since the program started.
        -> IO ()

animateIO display backColor frameFunIO
        = animateWithBackendIO 
                defaultBackendState display backColor
                frameFunIO
