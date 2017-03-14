{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import Solver
import Data.Array.Repa.IO.BMP
import System.Exit
import System.Environment
import Data.Maybe
import Data.Char


main :: IO ()
main 
 = do   args            <- getArgs
        config          <- parseArgs args defaultConfig

        (width,height) 
         <- if configDisplay config == FullScreen
             then getScreenSize
             else return (configSizeX config, configSizeY config)

        let world       = configPreset config
                          $ (initWorld width height)
                            { worldPixelsDynamic = configPixelsDynamic config}

        case configFileName config of
         -- Run interactively.
         Nothing
          -> playIO  (configDisplay config)
                black
                100
                (updateWorld world)
                draw handle advance

         -- Render image and write to .bmp file.
         Just filePath
          -> do arr     <- mandelArray  
                                (worldSizeX world) (worldSizeY  world)
                                (worldPosX  world) (worldPosY   world)
                                (worldZoom  world) (worldRadius world)
                                (truncate $ worldIterations world)

                writeImageToBMP filePath arr


-- Config ---------------------------------------------------------------------
data Config 
        = Config
        { configDisplay         :: Display 
        , configFileName        :: Maybe FilePath
        , configPreset          :: World -> World
        , configPixelsDynamic   :: Int
        , configSizeX           :: Int
        , configSizeY           :: Int }


defaultConfig :: Config
defaultConfig
        = Config
        { configDisplay         = InWindow "Mandelbrot" (800, 600) (10, 10) 
        , configFileName        = Nothing
        , configPreset          = id
        , configPixelsDynamic   = 4
        , configSizeX           = 800
        , configSizeY           = 600 }


parseArgs :: [String] -> Config -> IO Config
parseArgs args config
        | []    <- args
        = return config

        | "-fullscreen" : rest <- args
        = parseArgs rest 
        $ config { configDisplay = FullScreen }

        | "-window" : sizeX : sizeY : rest <- args
        , all isDigit sizeX
        , all isDigit sizeY
        = parseArgs rest
        $ config { configDisplay = InWindow "MandelBrot" (read sizeX, read sizeY) (0, 0)
                 , configSizeX   = read sizeX
                 , configSizeY   = read sizeY }

        | "-bmp" : sizeX : sizeY : fileName : rest <- args
        , all isDigit sizeX
        , all isDigit sizeY
        = parseArgs rest
        $ config { configFileName = Just fileName
                 , configSizeX    = read sizeX
                 , configSizeY    = read sizeY }

        | "-dynamic" : num : rest <- args
        , all isDigit num
        = parseArgs rest
        $ config { configPixelsDynamic = read num }

        | "-preset" : num : rest <- args
        , length num == 1
        , all isDigit num
        = parseArgs rest
        $ config { configPreset  = presets !! read num }

        | otherwise
        = do    printUsage
                exitWith $ ExitFailure 1

        
printUsage :: IO ()
printUsage
 = putStrLn 
        $ unlines
        [ "Usage: gloss-mandel [flags]"
        , "  -fullscreen"
        , "  -window      <width::INT> <height::INT>" 
        , "  -bmp         <width::INT> <height::INT> <FILE>" 
        , "  -dynamic     <INT>   Level of detail reduction when zooming and panning. (4) "
        , ""
        , " Controls:"
        , "  ESC                  Quit"
        , "  mouse drag           Centerpoint"
        , "  w/s                  Zoom"
        , "  a/d                  Maximum interations"
        , "  q/e                  Pixels per point"
        , "  z/c                  Escape radius for iteration"
        , "  0 .. 9               Select presets"
        , "  r                    Reset"
        , "  .                    Print current location to stdout" ]



-- World ----------------------------------------------------------------------
data World
        = World
        { worldPicture          :: Picture
        , worldSizeX            :: Int
        , worldSizeY            :: Int
        , worldPixels           :: Int
        , worldPixelsDynamic    :: Int

        , worldPosX             :: Double
        , worldPosY             :: Double 
        , worldZoom             :: Double

        , worldIterations       :: Double
        , worldRadius           :: Double

        , worldDragging         :: Maybe (Float, Float) 
        , worldZooming          :: Maybe Double } 


initWorld :: Int -> Int -> World
initWorld sizeX sizeY 
 = World
        { worldPicture          = Blank

        , worldSizeX            = sizeX
        , worldSizeY            = sizeY
        , worldPixels           = 1
        , worldPixelsDynamic    = 4

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
               y'      = 2 * (f2d (y0 - y)) * worldZoom world / (fromIntegral $ worldSizeX world)
          in   return  $ moveWorld x' y'
                       $ world { worldDragging = Just (x, y) }

        -- Zoom
        | EventKey (Char 's') Down   _ _      <- event
        = return $ world { worldZooming = Just 1.01 }

        | EventKey (Char 'w') Down   _ _      <- event
        = return $ world { worldZooming = Just 0.99 }

        -- Iterations
        | EventKey (Char 'a')  Down   _ _      <- event
        , iters         <- worldIterations world * 0.8
        , iters'        <- if iters < 1 then 1 else iters
        = return $ world { worldIterations = iters' }

        | EventKey (Char 'd') Down   _ _      <- event
        = return $ world { worldIterations = worldIterations world * 1.2 }

        -- Radius 
        | EventKey (Char 'z')  Down   _ _      <- event
        = return $ world { worldRadius = worldRadius world * 0.5 }

        | EventKey (Char 'c')  Down   _ _      <- event
        = return $ world { worldRadius = worldRadius world * 2 }

        -- Pixels
        | EventKey (Char 'q')  Down   _ _      <- event
        , worldPixels world > 1
        = return $ world { worldPixels = worldPixels world - 1 }

        | EventKey (Char 'e')  Down   _ _      <- event
        = return $ world { worldPixels = worldPixels world + 1 }

        -- Reset
        | EventKey (Char 'r')  Down   _ _       <- event
        = return $ initWorld (worldSizeX world) (worldSizeY world)

        -- Dump preset
        | EventKey (Char 'p')  Down  _  _       <- event
        = do    putStrLn $ showWorld world
                return world

        -- Load preset
        | EventKey (Char d)   Down _ _          <- event
        , isDigit d
        = return $ updateWorld ((presets !! read [d]) world)

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
 = let  dynamic         =  isJust (worldDragging  world)
                        || isJust (worldZooming   world)

        pixels
         | dynamic      = worldPixels world + worldPixelsDynamic world
         | otherwise    = worldPixels world

   in   world   { worldPicture  
                = mandelPicture
                        (worldSizeX world) (worldSizeY world)
                        pixels pixels
                        (worldPosX  world) (worldPosY  world)
                        (worldZoom  world) 
                        (worldRadius world)
                        (truncate $ worldIterations world)
                }


-- Presets --------------------------------------------------------------------
-- | Show the current state of the world, in preset form.
showWorld :: World -> String
showWorld world
 = show ( worldPosX world
        , worldPosY world
        , worldZoom world
        , worldIterations world
        , worldRadius world)


-- | Load a preset into the world.
loadWorld :: (Double, Double, Double, Double, Double) -> World -> World
loadWorld (posX, posY, zoom, iters, radius) world
        = world
        { worldPosX             = posX
        , worldPosY             = posY
        , worldZoom             = zoom
        , worldIterations       = iters
        , worldRadius           = radius }


presets :: [World -> World]
presets 
 = map loadWorld 
 $ [ (-0.5, 0, 2, 100, 2)
   , (0.20508818500545423,   0.9014915666351141   * 900/1440,6.375321937544527e-6, 629.3354966759534,  16.0)
   , (0.4510757067879078,    0.6144133202705898   * 900/1440,7.632248223018773e-5, 253.61352386150395, 2.0)
   , (0.3469337523117071,    0.6866350870407725   * 900/1440,3.508380713647269e-5,  168.61054759193718, 1024.0)
   , (-0.7902001921590814,   0.24910667566731381  * 900/1440,5.071115028132377e-4,  1176.757810813391,  3.4359738368e10)
   , (2.3127178455019423e-2,-1.301205470975472    * 900/1440,3.6349313304610088e-9, 343.0390372557315,  2.0)
   , (2.3127176148480418e-2,-1.3012054707668765   * 900/1440,2.71444790387451e-10,  604.1620768089155,  2.0)
   , (2.3127176156746785e-2,-1.301205470242045    * 900/1440,4.49615119202067e-12,  1731.8575629678642, 2.0)
   , (0.2550376327692795,    8.962363618058007e-4 * 900/1440,7.351698819132829e-5,  1412.1093729760698, 16.0)
   , (0.25498593633806477,   8.726424280526077e-4 * 900/1440,1.6858526052251987e-10,10492.090844482025, 2.0) ]

