
module QuadTree 
        ( QuadTree(..)
        , treeZero
        , treeInsert
        , treeElems )
where
import Graphics.Gloss.Data.Point

data QuadTree a
        -- Nil cells take up space in the world, but don't contain any elements.
        --      They can be at any depth in the tree.
        = QNil  !Point          -- cell center point 
                !Float          -- cell size

        -- Leaf cells are the only ones that contain elements.
        --      They are always at the bottom of the tree.
        | QLeaf !Point          -- cell center point 
                !Float          -- cell size
                ![a]            -- elements in this cell

        -- Node cells contain more sub-trees
        | QNode !Point          -- cell center point
                !Float          -- cell size
                !(QuadTree a) !(QuadTree a)     -- NW NE
                !(QuadTree a) !(QuadTree a)     -- SW SE
                
        deriving (Eq, Show)


-- Initial ----------------------------------------------------------------------------------------
treeZero size
        = QNil (0, 0) size

-- Quadrant ---------------------------------------------------------------------------------------

-- | Insert an element with a bounding box into the tree
treeInsert 
        :: Int          -- ^ maximum depth to place a leaf
        -> Int          -- ^ current depth
        -> Point        -- ^ bottom left of bounding box of new element
        -> Point        -- ^ top right of bounding box of new element
        -> a            -- ^ element to insert into tree
        -> QuadTree a   -- ^ current tree
        -> QuadTree a

treeInsert depthMax depth p0@(x0, y0) p1@(x1, y1) a tree
 = case tree of
        QNode p@(x, y) size tNW tNE tSW tSE
         -> let 
         
                tNW'    | y1 > y && x0 < x      = treeInsert depthMax (depth + 1) p0 p1 a tNW
                        | otherwise             = tNW

                tNE'    | y1 > y && x1 > x      = treeInsert depthMax (depth + 1) p0 p1 a tNE
                        | otherwise             = tNE

                tSW'    | y0 < y && x0 < x      = treeInsert depthMax (depth + 1) p0 p1 a tSW
                        | otherwise             = tSW

                tSE'    | y0 < y && x1 > x      = treeInsert depthMax (depth + 1) p0 p1 a tSE
                        | otherwise             = tSE
                
            in  QNode p size tNW' tNE' tSW' tSE'
                
        QLeaf p@(x, y) size elems
         | depth >= depthMax
         -> QLeaf p size (a : elems)
         
        QNil p@(x, y) size
         | depth >= depthMax
         -> QLeaf p size [a]
         
         | otherwise
         -> treeInsert depthMax depth p0 p1 a
                (let s2 = size / 2
                 in  QNode p size 
                        (QNil (x - s2, y + s2) s2) (QNil (x + s2, y + s2) s2)
                        (QNil (x - s2, y - s2) s2) (QNil (x + s2, y - s2) s2))


-- flatten a quadtree into a list of its elements.
treeElems :: QuadTree a -> [[a]]
treeElems tree 
 = case tree of
        QNode _ _ tNW tNE tSW tSE
         -> treeElems tNW ++ treeElems tNE ++ treeElems tSW ++ treeElems tSE
        
        QLeaf _ _ elems  -> [elems]
        QNil{}           -> []
