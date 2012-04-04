{-# LANGUAGE BangPatterns, PatternGuards #-}
import World
import Trace
import Light
import Object
import Vec3
import System.Environment
import qualified Graphics.Gloss                         as G
import qualified Graphics.Gloss.Interface.Pure.Game     as G
import qualified Graphics.Gloss.Raster.Field            as G

main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         []     -> run 800 600 4 100 4

         [sizeX, sizeY, zoom, fov, bounces]
                -> run (read sizeX) (read sizeY) (read zoom) (read fov) (read bounces)

         _ -> putStr $ unlines
           [ "trace <sizeX::Int> <sizeY::Int> <zoom::Int> (fov::Int) (bounces::Int)"
           , "    sizeX, sizeY - visualisation size        (default 800, 600)"
           , "    zoom         - pixel replication factor  (default 4)"
           , "    fov          - field of view             (default 100)"
           , "    bounces      - ray bounce limit          (default 4)"
           , ""
           , " You'll want to run this with +RTS -N to enable threads" ]
   

-- | World and interface state.
data State
        = State
        { stateTime             :: !Float 
        , stateEyePos           :: !Vec3
        , stateEyeLoc           :: !Vec3

        , stateLeftClick        :: !(Maybe G.Point)

        , stateMoveSpeed        :: !Float
        , stateMovingForward    :: !Bool
        , stateMovingBackward   :: !Bool
        , stateMovingLeft       :: !Bool
        , stateMovingRight      :: !Bool

        , stateObjects          :: ![Object]
        , stateObjectsView      :: ![Object]

        , stateLights           :: ![Light]
        , stateLightsView       :: ![Light] }

        deriving (Eq, Show)


-- | Initial world and interface state.
initState :: State
initState
        = State
        { stateTime             = 0
        , stateEyePos           = Vec3 50    (-100) (-700)
        , stateEyeLoc           = Vec3 (-50) 200   1296

        , stateLeftClick        = Nothing 

        , stateMoveSpeed        = 400
        , stateMovingForward    = False
        , stateMovingBackward   = False
        , stateMovingLeft       = False
        , stateMovingRight      = False

        , stateObjects          = makeObjects 0
        , stateObjectsView      = makeObjects 0

        , stateLights           = makeLights  0
        , stateLightsView       = makeLights  0 }


-- | Run the game.
run :: Int -> Int -> Int -> Int -> Int -> IO ()                     
run sizeX sizeY zoom fov bounces
 = G.playField 
        (G.InWindow "Ray" (sizeX, sizeY) (10, 10))
        (zoom, zoom)
        100
        initState
        (tracePixel sizeX sizeY fov bounces)
        handleEvent
        advanceState
{-# NOINLINE run #-}


-- | Render a single pixel of the image.
tracePixel :: Int -> Int -> Int -> Int -> State -> G.Point -> G.Color
tracePixel !sizeX !sizeY !fov !bounces !state (x, y)
 = let  !sizeX'  = fromIntegral sizeX
        !sizeY'  = fromIntegral sizeY
        !aspect  = sizeX' / sizeY'
        !fov'    = fromIntegral fov
        !fovX    = fov' * aspect
        !fovY    = fov'
       
        !ambient = Vec3 0.3 0.3 0.3
        !eyePos  = stateEyePos state
        !eyeDir  = normaliseV3 ((Vec3 (x * fovX) ((-y) * fovY) 0) - eyePos)

        Vec3 r g b
          = traceRay    (stateObjectsView state) 
                        (stateLightsView  state) ambient
                        eyePos eyeDir
                        bounces

   in   G.rawColor r g b 1.0
{-# INLINE tracePixel #-}


-- | Handle an event from the user interface.
handleEvent :: G.Event -> State -> State
handleEvent event state 
        -- Start translation.
        | G.EventKey (G.MouseButton G.LeftButton) 
                     G.Down _ (x, y) <- event
        = state { stateLeftClick = Just (x, y)}

        -- End transation.
        | G.EventKey (G.MouseButton G.LeftButton) 
                     G.Up _ _ <- event
        = state { stateLeftClick = Nothing }

        -- Moving forward
        | G.EventKey (G.Char 'w') G.Down _ _        <- event
        = state { stateMovingForward  = True }

        | G.EventKey (G.Char 'w') G.Up   _ _        <- event
        = state { stateMovingForward  = False }

        -- Moving backward
        | G.EventKey (G.Char 's') G.Down _ _        <- event
        = state { stateMovingBackward = True }

        | G.EventKey (G.Char 's') G.Up   _ _        <- event
        = state { stateMovingBackward = False }

        -- Moving left
        | G.EventKey (G.Char 'a') G.Down _ _        <- event
        = state { stateMovingLeft = True }

        | G.EventKey (G.Char 'a') G.Up   _ _        <- event
        = state { stateMovingLeft = False }

        -- Moving right
        | G.EventKey (G.Char 'd') G.Down _ _        <- event
        = state { stateMovingRight = True }

        | G.EventKey (G.Char 'd') G.Up   _ _        <- event
        = state { stateMovingRight = False }

        -- Translate the world.
        | G.EventMotion (x, y)  <- event
        , Just (oX, oY)         <- stateLeftClick state
        , Vec3 eyeX eyeY eyeZ   <- stateEyeLoc    state
        = let   eyeX'   = eyeX + (x - oX)
                eyeY'   = eyeY
                eyeZ'   = eyeZ + (y - oY)

          in    setEyeLoc (Vec3 eyeX' eyeY' eyeZ')
                 $ state { stateLeftClick  = Just (x, y) }
        
        | otherwise
        = state
{-# NOINLINE handleEvent #-}


-- | Advance the world forward in time.
advanceState :: Float -> State -> State
advanceState advTime state
 = let  time'   = stateTime state + advTime

        speed   = stateMoveSpeed state
        move    = (if stateMovingForward state 
                        then moveEyeLoc (Vec3 0 0 (-speed * advTime))
                        else id)
                . (if stateMovingBackward state
                        then moveEyeLoc (Vec3 0 0 (speed * advTime))
                        else id)
                . (if stateMovingLeft state
                        then moveEyeLoc (Vec3 (speed * advTime) 0 0)
                        else id)
                . (if stateMovingRight state
                        then moveEyeLoc (Vec3 (-speed * advTime) 0 0)
                        else id)

   in   setTime time' $ move state
{-# NOINLINE advanceState #-}


-- | Set the location of the eye.
setEyeLoc :: Vec3 -> State -> State
setEyeLoc eyeLoc state
 = let  objects = makeObjects (stateTime state)
        lights  = makeLights  (stateTime state)
   in state 
        { stateEyeLoc           = eyeLoc
        , stateObjectsView      = map (translateObject (stateEyeLoc state)) objects
        , stateLightsView       = map (translateLight  (stateEyeLoc state)) lights 
        }
{-# NOINLINE setEyeLoc #-}


moveEyeLoc :: Vec3 -> State -> State
moveEyeLoc v state
 = let  objects = stateObjects state
        lights  = stateLights  state
        eyeLoc  = stateEyeLoc  state + v
   in state
        { stateEyeLoc           = eyeLoc
        , stateObjectsView      = map (translateObject eyeLoc) objects
        , stateLightsView       = map (translateLight  eyeLoc) lights
        }
{-# NOINLINE moveEyeLoc #-}


-- | Set the time of the world.
setTime   :: Float -> State -> State
setTime time state
 = let  objects = makeObjects time
        lights  = makeLights  time
   in state 
        { stateTime             = time
        , stateObjects          = objects
        , stateObjectsView      = map (translateObject (stateEyeLoc state)) objects

        , stateLights           = lights
        , stateLightsView       = map (translateLight  (stateEyeLoc state)) lights 
        }
{-# NOINLINE setTime #-}
