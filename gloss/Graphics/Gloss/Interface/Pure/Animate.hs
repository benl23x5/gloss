
-- | Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.Pure.Animate
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , animate)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Animate
import Graphics.Gloss.Internals.Interface.Backend


-- | Open a new window and display the given animation.
--
--   Once the window is open you can use the same commands as with `display`.
--
animate :: Display              -- ^ Display mode.
        -> Color                -- ^ Background color.
        -> (Float -> Picture)   -- ^ Function to produce the next frame of animation.
                                --      It is passed the time in seconds since the program started.
        -> IO ()

animate display backColor frameFun
        = animateWithBackendIO
                defaultBackendState
                True            -- pannable
                display backColor
                (return . frameFun)
                (const (return ()))
