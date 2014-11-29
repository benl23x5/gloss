module Graphics.Gloss.Render 
        ( render
        , renderAction
        , renderPicture
        , RS.stateInit)
where
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Gloss.Internals.Render.Common
import Graphics.Gloss.Internals.Render.Picture
import Graphics.Gloss.Internals.Color
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss.Internals.Render.State as RS
import System.Mem (performGC)


-- | Clear the current OpenGL context and draw the given picture into it. The
--   mutable state holds references to the textures currently loaded into the
--   context, and may have new ones added to it when drawing the picture.
render  :: RS.State     -- ^ Current rendering state.
        -> (Int, Int)   -- ^ Window width and height.
        -> Color        -- ^ Color to clear the window with.
        -> Picture      -- ^ Picture to draw.
        -> IO ()

render renderS windowSize clearColor picture
  = do 
        let viewPort = viewPortInit

        -- initialization (done every time in this case)
        -- we don't need the depth buffer for 2d.
        GL.depthFunc    GL.$= Just GL.Always

        -- always clear the buffer to white
        GL.clearColor   GL.$= glColor4OfColor clearColor

        -- on every loop
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

        renderAction
              windowSize
              (renderPicture renderS viewPort picture) 

        performGC
