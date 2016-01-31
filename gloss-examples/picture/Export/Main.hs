{-# LANGUAGE PackageImports #-}
import Codec.Picture.Types (Image(..), PixelRGBA8)
import Codec.Picture.Png (writePng)
import Control.Monad (forM_)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Rendering as Gloss
import Graphics.Rendering.OpenGL.Raw -- as gl*
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Foreign (ForeignPtr, newForeignPtr_)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import Text.Printf (printf)


windowWidth, windowHeight :: Num a => a
windowWidth = 10
windowHeight = 10

-- let GLFW bother with the OpenGL initialization
initOpenGL :: IO ()
initOpenGL = do
    True <- GLFW.init
    Just w <- GLFW.createWindow
                windowWidth windowHeight
                "gloss-to-file demo"
                Nothing Nothing
    GLFW.makeContextCurrent (Just w)

drawFrame :: Gloss.State -> Gloss.Picture -> IO ()
drawFrame s p = Gloss.withClearBuffer Gloss.black
            $ Gloss.withModelview (windowWidth, windowHeight)
            $ do
    glColor3f 1 1 1
    Gloss.renderPicture s 1 p

initialize :: IO Gloss.State
initialize = do
    s <- Gloss.initState
    initOpenGL
    return s

saveFrameImpl :: Gloss.State -> FilePath -> Gloss.Picture -> IO ()
saveFrameImpl s f p = do
    glDrawBuffer gl_BACK
    drawFrame s p
    glReadBuffer gl_BACK
    imageData <- mallocArray (windowWidth * windowHeight * 4)
    glReadPixels 0 0 windowWidth windowHeight gl_RGBA gl_UNSIGNED_BYTE imageData
    
    -- save the result
    foreignPtr <- newForeignPtr_ imageData
    let vector = unsafeFromForeignPtr0 foreignPtr (windowWidth * windowHeight * 4)
    let image :: Image PixelRGBA8
        image = Image windowWidth windowHeight vector
    printf "Writing frame to %s\n" f
    writePng f (Image windowWidth windowHeight vector :: Image PixelRGBA8)
    
    free imageData


saveFrame :: FilePath -> Gloss.Picture -> IO ()
saveFrame f p = do
    s <- initialize
    saveFrameImpl s f p


type Animation = Float -> Gloss.Picture

-- FilePath must contain "%d", will be replaced by frame number
saveFrames :: FilePath -> Animation -> [Float] -> IO ()
saveFrames f anim ts = do
    s <- initialize
    forM_ (zip [1..] ts) $ \(n, t) -> do
      let filename = printf f (n :: Int)
      let picture = anim t
      saveFrameImpl s filename picture


main :: IO ()
main = do
    saveFrame "circle.png" (Gloss.circle 5)
    saveFrames "growing_circle%d.png" Gloss.circle [1..5]
