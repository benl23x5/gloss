
-- | Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.Pure.Display
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , display)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Display
import Graphics.Gloss.Internals.Interface.Backend


-- | Open a new window and display the given picture.
display :: Display          -- ^ Display mode.
        -> Color            -- ^ Background color.
        -> Picture          -- ^ The picture to draw.
        -> IO ()

display dis backColor picture
        = displayWithBackend
                defaultBackendState
                dis
                backColor
                (return picture)
                (const (return ()))
