{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Graphics.Gloss.Interface.IO.Game
import Solver
import Data.Maybe

main :: IO ()
main 
 = let  
--        display = (FullScreen (sizeX, sizeY))

--        sizeX   = 800
--        sizeY   = 800
--        display = InWindow "mandel" (sizeX, sizeY) (0, 0)

        sizeX   = 1440
        sizeY   = 900
        display = FullScreen (sizeX, sizeY)

        x0  :: Double    =  0.001643721971153
        y0  :: Double    = -0.822467633298876

   in   playIO  display
                black
                100
                (initWorld sizeX sizeY x0 y0)
                draw handle advance


-- World ----------------------------------------------------------------------
data World
        = World
        { worldPicture          :: Picture
        , worldSizeX            :: Int
        , worldSizeY            :: Int
        , worldPixels           :: Int

        , worldPosX             :: Double
        , worldPosY             :: Double 
        , worldZoom             :: Double

        , worldIterations       :: Double
        , worldRadius           :: Double

        , worldDragging         :: Maybe (Float, Float) 
        , worldZooming          :: Maybe Double } 


initWorld :: Int -> Int -> Double -> Double -> World
initWorld sizeX sizeY posX posY
 = updateWorld
 $ World
        { worldPicture          = Blank

        , worldSizeX            = sizeX
        , worldSizeY            = sizeY
        , worldPixels           = 1

        , worldPosX             = posX
        , worldPosY             = posY 
        , worldZoom             = 1

        , worldIterations       = 100
        , worldRadius           = 2
        , worldDragging         = Nothing 
        , worldZooming          = Nothing }


draw :: World -> IO Picture
draw world  
        = return $ worldPicture world


handle :: Event -> World -> IO World
handle event world 

        -- Pan
        | EventKey (MouseButton LeftButton) Down _ (x, y) <- event
        = return $ updateWorld $ world { worldDragging = Just (x, y)}

        | EventKey (MouseButton LeftButton) Up   _ _      <- event
        = return $ updateWorld $ world { worldDragging = Nothing }

        | EventMotion (x, y)   <- event
        , Just (x0, y0)        <- worldDragging world
        = let  x'      = 2 * (f2d (x0 - x)) * worldZoom world / (fromIntegral $ worldSizeX world)
               y'      = 2 * (f2d (y0 - y)) * worldZoom world / (fromIntegral $ worldSizeY world)
          in   return  $ moveWorld x' y'
                       $ world { worldDragging = Just (x, y) }

        -- Zoom
        | EventKey (SpecialKey KeyDown) Down   _ _      <- event
        = return $ world { worldZooming = Just 1.01 }

        | EventKey (SpecialKey KeyUp)   Down   _ _      <- event
        = return $ world { worldZooming = Just 0.99 }

        -- Iterations
        | EventKey (SpecialKey KeyLeft)  Down   _ _      <- event
        = return $ world { worldIterations = worldIterations world * 0.8 }

        | EventKey (SpecialKey KeyRight) Down   _ _      <- event
        = return $ world { worldIterations = worldIterations world * 1.2 }

        -- Radius 
        | EventKey (Char 'z')  Down   _ _      <- event
        = return $ world { worldRadius = worldRadius world * 0.5 }

        | EventKey (Char 'x')  Down   _ _      <- event
        = return $ world { worldRadius = worldRadius world * 2 }

        -- Pixels
        | EventKey (Char 'a')  Down   _ _      <- event
        , worldPixels world > 1
        = return $ world { worldPixels = worldPixels world - 1 }

        | EventKey (Char 's')  Down   _ _      <- event
        = return $ world { worldPixels = worldPixels world + 1 }


        -- Cancel zoom
        | EventKey _   Up   _ _      <- event
        = return $ updateWorld $ world { worldZooming = Nothing }

        | otherwise
        = return world


advance :: Float -> World -> IO World
advance _ world 
        | Just factor   <- worldZooming world
        = return $ zoomWorld factor world

        | otherwise
        = return world


moveWorld :: Double -> Double -> World -> World
moveWorld bumpX bumpY world
 = updateWorld
 $ world        { worldPosX     = worldPosX world + bumpX
                , worldPosY     = worldPosY world + bumpY }

zoomWorld :: Double -> World -> World
zoomWorld zoom world
 = updateWorld
 $ world        { worldZoom     = worldZoom world * zoom }


updateWorld :: World -> World
updateWorld world
 = let  dynamic =  isJust (worldDragging  world)
                || isJust (worldZooming   world)

        pixels
         | dynamic                      = worldPixels world + 4
         | otherwise                    = worldPixels world

   in   world { worldPicture  
                = mandelFrame 
                        (worldSizeX world) (worldSizeY world)
                        pixels pixels
                        (worldPosX  world) (worldPosY  world)
                        (worldZoom  world) 
                        (worldRadius world)
                        (truncate $ worldIterations world)
                }

