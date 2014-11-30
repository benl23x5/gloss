
module Graphics.Gloss.Rendering 
        ( -- * Picture data type
          Picture (..)
        , Point, Vector, Path

          -- * Colors
        , Color
        , makeColor
        , makeColorI
        , makeRawColor
        , makeRawColorI
        , rgbaOfColor
        , clampColor

          -- * Bitmaps
        , BitmapData
        , bitmapOfForeignPtr
        , bitmapOfByteString
        , bitmapOfBMP
        , loadBMP

          -- * Rendering
        , displayPicture
        , renderPicture
        , withModelview
        , withClearBuffer
        , RS.initState)

where
import Graphics.Gloss.Internals.Rendering.Common
import Graphics.Gloss.Internals.Rendering.Picture
import Graphics.Gloss.Internals.Data.Picture
import Graphics.Gloss.Internals.Data.Color
import qualified Graphics.Gloss.Internals.Rendering.State as RS


-- | Set up the OpenGL context, clear the buffer, and render the given picture
--   into it. 
--
--   This is the same as `renderPicture` composed with `withModelview`
--   and `withClearBuffer`. If you want to manage your own OpenGL context then
--   you can just call `renderPicture`. 
--
--   Using this function assumes that you've already opened a window
--   and set that to the active context. If you don't want to do your own window
--   management then use the @gloss@ package instead.
displayPicture
        :: (Int, Int)   -- ^ Window width and height.
        -> Color        -- ^ Color to clear the window with.
        -> RS.State     -- ^ Current rendering state.
        -> Float        -- ^ View port scale, which controls the level of detail.
                        --   Use 1.0 to start with.
        -> Picture      -- ^ Picture to draw.
        -> IO ()

displayPicture windowSize colorClear state scale picture
  = withModelview      windowSize
  $ withClearBuffer    colorClear
  $ renderPicture  state scale picture
