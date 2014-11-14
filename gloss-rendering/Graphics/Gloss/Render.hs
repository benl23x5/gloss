module Graphics.Gloss.Render (
    render
  , renderAction
  , renderPicture
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Gloss.Internals.Render.Common
import Graphics.Gloss.Internals.Render.Picture
import Graphics.Gloss.Internals.Color
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss.Internals.Render.State as RS

render :: (Int, Int) -> Color -> Picture -> IO ()
render windowSize
       clearColor
       picture
  = do renderS <- RS.stateInit
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
