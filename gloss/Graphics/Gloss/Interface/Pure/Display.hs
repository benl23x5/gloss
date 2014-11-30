
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
--
--   Use the following commands once the window is open:
--
--      * Quit - esc-key.
--      * Move Viewport - left-click drag, arrow keys.
--      * Rotate Viewport - right-click drag, control-left-click drag, or home\/end-keys.
--      * Zoom Viewport - mouse wheel, or page up\/down-keys.
--
display :: Display          -- ^ Display mode.
        -> Color            -- ^ Background color.
        -> Picture          -- ^ The picture to draw.
        -> IO ()

display = displayWithBackend defaultBackendState
