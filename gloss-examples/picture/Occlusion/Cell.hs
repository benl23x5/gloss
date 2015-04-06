
module Cell
        ( Cell (..)
        , readCell 
        , pictureOfCell
        , cellShape)
where
import Data.Char
import Graphics.Gloss

-- | A terrain cell in the world.
data Cell
        = CellEmpty
        | CellWall
        deriving (Show, Eq)


-- | Read a cell from a character.
readCell :: Char -> Cell
readCell c
 = case c of
        '.'     -> CellEmpty
        '#'     -> CellWall
        _       -> error $ "readCell: no match for char " ++ show (ord c) ++ " " ++ show c


-- | The basic shape of a cell.
cellShape :: Int -> Int -> Int -> Picture
cellShape cellSize posXi posYi
 = let  cs      = fromIntegral cellSize
        posX    = fromIntegral posXi
        posY    = fromIntegral posYi
        x1      = posX
        x2      = posX + 1
        y1      = posY 
        y2      = posY + 1
   in   Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
                

-- | Convert a cell to a picture, based on a primitive shape.
--      We pass the shape in to avoid recomputing it for each cell.
pictureOfCell ::  Int -> Int -> Int -> Cell -> Picture
pictureOfCell cellSize posX posY cell
 = case cell of
        CellEmpty       -> Color (greyN 0.2)    (cellShape cellSize posX posY)
        CellWall        -> Color white          (cellShape cellSize posX posY)

