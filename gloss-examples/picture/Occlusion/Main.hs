{-# LANGUAGE PatternGuards #-}

import World
import Data
import State
import Cell
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.QuadTree
import Graphics.Gloss.Data.Extent
import System.Environment
import Data.Maybe
import Data.List
import Data.Function

main 
 = do   args    <- getArgs
        case args of
         [fileName]     
          -> do world   <- loadWorld fileName
                mainWithWorld world
                
         _ -> do
                let world = readWorld worldData
                mainWithWorld world
        
        
mainWithWorld world
 = play (InWindow "Occlusion"
                 (windowSizeOfWorld world) (10, 10))
        black 
        10
        (initState world)
        drawState
        (handleInput world)
        (\_ -> id)
                                
                                
-- | Convert the state to a picture.
drawState :: State -> Picture
drawState state
 = let  world           = stateWorld state

        -- The ray cast by the user.
        p1              = stateLineStart state
        p2              = stateLineEnd   state
        picRay          = drawRay world p1 p2

        -- The cell hit by the ray (if any)
        mHitCell        = castSegIntoWorld world p1 p2
        hitCells        = maybeToList mHitCell
        picCellsHit     = Pictures $ map (drawHitCell world) hitCells

        -- All the cells in the world.
        cellsAll        = flattenQuadTree (worldExtent world) (worldTree world)
        picCellsAll     = Pictures $ map (uncurry (drawCell False world)) cellsAll

        -- The cells visible from the designated point.
        cellsVisible    
          = [ (coord, cell)
                | (coord, cell) <- flattenQuadTree (worldExtent world) (worldTree world)
                , cellAtCoordIsVisibleFromPoint world p1 coord ]

        picCellsVisible = Pictures $ map (uncurry (drawCell True world)) cellsVisible

        -- How big to draw the cells.
        scale           = fromIntegral $ worldCellSize world

        (windowSizeX, windowSizeY)      
                = windowSizeOfWorld
                $ stateWorld state
                
        -- Shift the cells so they are centered in the window.
        offsetX = - (fromIntegral $ windowSizeX `div` 2)
        offsetY = - (fromIntegral $ windowSizeY `div` 2)

   in   Translate offsetX offsetY
                $ Scale scale scale
                $ Pictures [ picCellsAll, picCellsVisible, picCellsHit, picRay ]


-- | Draw the cell hit by the ray defined by the user.
drawHitCell :: World -> (Point, Extent, Cell) -> Picture
drawHitCell world (pos@(px, py), extent, cell)
 = let  (n, s, e, w)    = takeExtent extent
        x               = w
        y               = s

        posX    = fromIntegral x 
        posY    = fromIntegral y
        
   in   Pictures [ Color blue $ cellShape 1 posX posY ]


-- | Draw the ray defined by the user.
drawRay :: World -> Point -> Point -> Picture 
drawRay world p1@(x, y) p2
 = Pictures
        [ Color red $ Line [p1, p2]
        , Color cyan 
                $ Translate x y 
                $ Pictures 
                        [ Line [(-0.3, -0.3), (0.3,  0.3)]
                        , Line [(-0.3,  0.3), (0.3, -0.3)] ] ]


-- | Draw a cell in the world.
drawCell :: Bool -> World -> Coord -> Cell -> Picture
drawCell visible world (x, y) cell 
 = let  cs      = fromIntegral (worldCellSize world)
        cp      = fromIntegral (worldCellSpace world)

        posX    = fromIntegral x 
        posY    = fromIntegral y

   in   if visible
          then pictureOfCell (worldCellSize world) posX posY cell
          else Color (greyN 0.4) (cellShape cs posX posY)
        
