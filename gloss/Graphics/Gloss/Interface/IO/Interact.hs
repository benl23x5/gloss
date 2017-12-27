
-- | Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.IO.Interact
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , interactIO
        , Controller    (..)
        , Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Event
import Graphics.Gloss.Internals.Interface.Interact
import Graphics.Gloss.Internals.Interface.Backend


-- | Open a new window and interact with an infrequently updated picture.
--
--   Similar to `displayIO`, except that you manage your own events.
--
interactIO
        :: Display                      -- ^ Display mode.
        -> Color                        -- ^ Background color.
        -> world                        -- ^ Initial world state.
        -> (world -> IO Picture)        -- ^ A function to produce the current picture.
        -> (Event -> world -> IO world) -- ^ A function to handle input events.
        -> (Controller -> IO ())        -- ^ Callback to take the display controller.
        -> IO ()

interactIO dis backColor worldInit makePicture handleEvent eatController
 =      interactWithBackend
                defaultBackendState
                dis
                backColor
                worldInit
                makePicture
                handleEvent
                eatController
