{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Graphics.Gloss.Interface.IO.Game
import Solver

main :: IO ()
main 
 = let  
--        display = (FullScreen (sizeX, sizeY))

        sizeX   = 800
        sizeY   = 800
        display = InWindow "mandel" (sizeX, sizeY) (0, 0)

        x0  :: Double    =  0.001643721971153
        y0  :: Double    = -0.822467633298876

   in   playIO  display
                black
                100
                (initWorld sizeX sizeY 1 1 x0 y0)
                draw handle advance


-- World ----------------------------------------------------------------------
data World
        = World
        { worldPicture  :: Picture
        , worldSizeX    :: Int
        , worldSizeY    :: Int
        , worldZoomX    :: Int
        , worldZoomY    :: Int
        , worldPosX     :: Double
        , worldPosY     :: Double 
        , worldDrag     :: Maybe (Float, Float) }


initWorld :: Int -> Int -> Int -> Int -> Double -> Double -> World
initWorld sizeX sizeY zoomX zoomY posX posY
 = updateWorld
 $ World
        { worldPicture  = Blank
        , worldSizeX    = sizeX
        , worldSizeY    = sizeY
        , worldZoomX    = zoomX
        , worldZoomY    = zoomY
        , worldPosX     = posX
        , worldPosY     = posY 
        , worldDrag     = Nothing }


draw :: World -> IO Picture
draw world  
        = return $ worldPicture world


handle :: Event -> World -> IO World
handle event world 
 | EventKey (MouseButton LeftButton) Down _ (x, y) <- event
 = return $ world { worldDrag = Just (x, y)}

 | EventKey (MouseButton LeftButton) Up   _ _      <- event
 = return $ world { worldDrag = Nothing }

 | EventMotion (x, y)   <- event
 , Just (x0, y0)        <- worldDrag world
 = let  x'      = 2 * (f2d (x0 - x)) / (fromIntegral $ worldSizeX world)
        y'      = 2 * (f2d (y0 - y)) / (fromIntegral $ worldSizeY world)
   in   return  $ moveWorld x' y'
                $ world { worldDrag = Just (x, y)}



 | otherwise
 = return world


advance :: Float -> World -> IO World
advance _ world 
        = return world


moveWorld :: Double -> Double -> World -> World
moveWorld bumpX bumpY world
 = updateWorld
 $ world        { worldPosX     = worldPosX world + bumpX
                , worldPosY     = worldPosY world + bumpY }

updateWorld :: World -> World
updateWorld world
 = world { worldPicture  
         = mandelFrame 
                (worldSizeX world) (worldSizeY world)
                (worldZoomX world) (worldZoomY world)
                (worldPosX  world) (worldPosY  world) }

