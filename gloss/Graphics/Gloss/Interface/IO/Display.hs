
-- | Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.IO.Display
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , displayIO
        , Controller    (..))
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Display
import Graphics.Gloss.Internals.Interface.Backend


-- | Open a new window and display an infrequently updated picture.
--
--   Once the window is open you can use the same commands as with @display@.
--
--   * This wrapper is intended for mostly static pictures that do not
--     need to be updated more than once per second. For example, the picture
--     could show network activity over the last minute, a daily stock price,
--     or a weather forecast. If you want to show a real-time animation where
--     the frames are redrawn more frequently then use the `animate` wrapper
--     instead.
--
--   * The provided picture generating action will be invoked, and the
--     display redrawn in two situation:
--     1) We receive a display event, like someone clicks on the window.
--     2) When `controllerSetRedraw` has been set, some indeterminate time
--     between the last redraw, and one second from that.
--
--   * Note that calling `controllerSetRedraw` indicates that the picture should
--     be redrawn, but does not cause this to happen immediately, due to
--     limitations in the GLUT and GLFW window managers. The display runs on
--     a one second timer interrupt, and if there have been no display events
--     we need to wait for the next timer interrupt before redrawing.
--     Having the timer interrupt period at 1 second keeps the CPU usage
--     due to the context switches at under 1%.
--
--   * Also note that the picture generating action is called for every display
--     event, so if the user pans the display then it will be invoked at 10hz
--     or more during the pan. If you are generating the picture by reading some
--     on-disk files then you should track when the files were last updated
--     and cache the picture between updates. Caching the picture avoids
--     repeatedly reading and re-parsing your files during a pan. Consider
--     storing your current picture in an IORef, passing an action that just
--     reads this IORef, and forking a new thread that watches your files for updates.
--
displayIO
        :: Display                -- ^ Display mode.
        -> Color                  -- ^ Background color.
        -> IO Picture             -- ^ Action to produce the current picture.
        -> (Controller -> IO ())  -- ^ Callback to take the display controller.
        -> IO ()

displayIO dis backColor makePicture eatController
 =      displayWithBackend
                defaultBackendState
                dis
                backColor
                makePicture
                eatController


