
module Main where
import World
import Cell
import Graphics.Gloss
import qualified Data.Vector    as Vec

main :: IO ()
main    
 = do   
        let width       = 150
        let height      = 100
        world   <- randomWorld (width, height)
        
        simulate (InWindow "John Conway's Game of Life" 
                           (windowSizeOfWorld world) (5, 5))
                white 10 world drawWorld simulateWorld
        

-- | Convert a world to a picture.
drawWorld
        :: World 
        -> Picture

drawWorld world 
 = let  (windowWidth, windowHeight)     
                = windowSizeOfWorld world
                
        offsetX = - fromIntegral windowWidth  / 2
        offsetY = - fromIntegral windowHeight / 2 
   in   Translate offsetX offsetY
                $ Pictures 
                $ Vec.toList 
                $ Vec.imap (drawCell world) (worldCells world)


-- | Convert a cell at a particular coordinate to a picture.
drawCell :: World -> Index -> Cell -> Picture
drawCell world index cell 
 = let  cs      = fromIntegral (worldCellSize world)
        cp      = fromIntegral (worldCellSpace world)

        (x, y)  = coordOfIndex world index
        fx      = fromIntegral x * (cs + cp) + 1
        fy      = fromIntegral y * (cs + cp) + 1

   in   pictureOfCell
                (worldCellOldAge world)
                (worldCellSize   world)
                fx
                fy
                cell
                

-- | Get the size of the window needed to display a world.
windowSizeOfWorld :: World -> (Int, Int)
windowSizeOfWorld world
 = let  cellSize        = worldCellSize world
        cellSpace       = worldCellSpace world
        cellPad         = cellSize + cellSpace
        height          = cellPad * (worldHeight world) + cellSpace
        width           = cellPad * (worldWidth  world) + cellSpace
   in   (width, height)

