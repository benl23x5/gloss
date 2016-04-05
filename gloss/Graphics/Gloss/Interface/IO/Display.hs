
-- | Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.IO.Display
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , displayIO)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Display
import Graphics.Gloss.Internals.Interface.Backend


-- | Open a new window and display the given animation.
--
--   Once the window is open you can use the same commands as with @display@.
--
displayIO
        :: Display      -- ^ Display mode.
        -> Color        -- ^ Background color.
        -> IO Picture   -- ^ Function to produce the current picture.
        -> IO ()

displayIO dis backColor makePicture
 =      displayWithBackend
                defaultBackendState
                dis
                backColor
                makePicture


