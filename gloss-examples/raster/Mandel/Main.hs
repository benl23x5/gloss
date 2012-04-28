{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Graphics.Gloss.Interface.IO.Game
import Solver
import Data.Maybe
import Data.Char

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

   in   playIO  display
                black
                100
                (initWorld sizeX sizeY)
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



initWorld :: Int -> Int -> World
initWorld sizeX sizeY 
 = updateWorld
 $ World
        { worldPicture          = Blank

        , worldSizeX            = sizeX
        , worldSizeY            = sizeY
        , worldPixels           = 1

        , worldPosX             = -0.5
        , worldPosY             = 0 
        , worldZoom             = 2

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
        , iters         <- worldIterations world * 0.8
        , iters'        <- if iters < 1 then 1 else iters
        = return $ world { worldIterations = iters' }

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

        -- Reset
        | EventKey (Char 'r')  Down   _ _       <- event
        = return $ initWorld (worldSizeX world) (worldSizeY world)

        -- Dump preset
        | EventKey (Char '.')  Down  _  _       <- event
        = do    putStrLn $ showWorld world
                return world

        -- Load preset
        | EventKey (Char d)   Down _ _          <- event
        , isDigit d
        = case d of
                '1'     -> return $ updateWorld $ preset1 world
                '2'     -> return $ updateWorld $ preset2 world
                '3'     -> return $ updateWorld $ preset3 world
                '4'     -> return $ updateWorld $ preset4 world
                '5'     -> return $ updateWorld $ preset5 world
                '6'     -> return $ updateWorld $ preset6 world
                '7'     -> return $ updateWorld $ preset7 world
                '8'     -> return $ updateWorld $ preset8 world
                '9'     -> return $ updateWorld $ preset9 world
                '0'     -> return $ updateWorld $ preset0 world
                _       -> return world

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


-- Presets --------------------------------------------------------------------
showWorld :: World -> String
showWorld world
 = show ( worldPosX world
        , worldPosY world
        , worldZoom world
        , worldIterations world
        , worldRadius world)


loadWorld :: (Double, Double, Double, Double, Double) -> World -> World
loadWorld (posX, posY, zoom, iters, radius) world
        = world
        { worldPosX             = posX
        , worldPosY             = posY
        , worldZoom             = zoom
        , worldIterations       = iters
        , worldRadius           = radius }


preset0, preset1, preset2, preset3, preset4, preset5, preset6, preset7, preset8, preset9 :: World -> World
preset0 = loadWorld (-0.5, 0, 2, 100, 2)
preset1 = loadWorld (0.20508818500545423,0.9014915666351141,6.375321937544527e-6,629.3354966759534,16.0)
preset2 = loadWorld (0.4510757067879078,0.6144133202705898,7.632248223018773e-5,253.61352386150395,2.0)
preset3 = loadWorld (0.3469337523117071,0.6866350870407725,3.508380713647269e-5,168.61054759193718,1024.0)
preset4 = loadWorld (-0.7902001921590814,0.24910667566731381,5.071115028132377e-4,1176.757810813391,3.4359738368e10)
preset5 = loadWorld (2.3127178455019423e-2,-1.301205470975472,3.6349313304610088e-9,343.0390372557315,2.0)
preset6 = loadWorld (2.3127176148480418e-2,-1.3012054707668765,2.71444790387451e-10,604.1620768089155,2.0)
preset7 = loadWorld (2.3127176156746785e-2,-1.301205470242045,4.49615119202067e-12,1731.8575629678642,2.0)
preset8 = loadWorld (0.2550376327692795,8.962363618058007e-4,7.351698819132829e-5,1412.1093729760698,16.0)
preset9 = loadWorld (0.25498593633806477,8.726424280526077e-4,1.6858526052251987e-10,10492.090844482025,2.0)

