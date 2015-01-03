{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)
import Graphics.Gloss.Rendering
import Graphics.Gloss


main :: IO ()
main = do
    let width  = 200
        height = 200

    state  <- initState

    withWindow width height "Render" $ \win -> do
        loop state win (width, height)

    where loop state window (w, h) = do
            threadDelay 20000
            pollEvents
            displayPicture (w, h) white state 1.0 (Circle 80)
            swapBuffers window
            k <- keyIsPressed window Key'Escape
            unless k $ loop state window (w, h)


withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]


keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key


isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False
