{-# LANGUAGE BangPatterns #-}
-- KDTree code
--   by Matthew Sottile <matt@galois.com> <mjsottile@computer.org>
--
module KDTree2d (
  KDTreeNode(..),
  newKDTree,
  kdtAddPoints,
  kdtAddPoint,
  kdtRangeSearch,
  kdtCollisionDetect,
  kdtInBounds,
  dumpKDTree,
  mapKDTree,
  kdtreeToList
) where
import Vec2
import Data.Maybe
import System.IO


data KDTreeNode a 
        = Empty
        | Node !(KDTreeNode a) !Vec2 !a !(KDTreeNode a)
        deriving Show


-- | An empty KDTree
newKDTree :: KDTreeNode a
newKDTree = Empty

-- | Flatten out a KDTree to a list.
kdtreeToList :: KDTreeNode a -> [a]
kdtreeToList Empty              = []
kdtreeToList (Node l _ x r)     = [x] ++ kdtreeToList l ++ kdtreeToList r


-- | Apply a worker function to all elements of a KDTree.
mapKDTree :: KDTreeNode a -> (a -> b) -> [b]
mapKDTree Empty _               = []
mapKDTree (Node l p n r) f      = f n : (mapKDTree l f ++ mapKDTree r f)


kdtAddWithDepth :: KDTreeNode a -> Vec2 -> a -> Int -> KDTreeNode a
kdtAddWithDepth Empty pos dat _ 
        = Node Empty pos dat Empty

kdtAddWithDepth (Node left npos ndata right) pos dat d 
        | vecDimSelect pos d < vecDimSelect npos d
        = Node (kdtAddWithDepth left pos dat d') npos ndata right

        | otherwise
        = Node left npos ndata (kdtAddWithDepth right pos dat d')
        where d' = if (d == 1) then 0 else 1


kdtAddPoint :: KDTreeNode a -> Vec2 -> a -> KDTreeNode a
kdtAddPoint t p d 
        = kdtAddWithDepth t p d 0

kdtInBounds p bMin bMax 
        = vecLessThan p bMax && vecGreaterThan p bMin


-- X dimension
kdtRangeSearchRecX :: KDTreeNode a -> Vec2 -> Vec2 -> [(Vec2,a)]
kdtRangeSearchRecX Empty _ _ = []
kdtRangeSearchRecX (Node left npos ndata right) bMin bMax
        | nc < mnc
        = nextfun right bMin bMax

        | nc > mxc
        = nextfun left bMin bMax

        | kdtInBounds npos bMin bMax
        = (npos, ndata) 
        : (nextfun right bMin bMax ++ nextfun left bMin bMax)

        | otherwise
        =  nextfun right bMin bMax ++ nextfun left bMin bMax

        where   Vec2 nc _       = npos
                Vec2 mnc _      = bMin
                Vec2 mxc _      = bMax
                nextfun         = kdtRangeSearchRecY


-- Y dimension
kdtRangeSearchRecY :: (KDTreeNode a) -> Vec2 -> Vec2 -> [(Vec2,a)]
kdtRangeSearchRecY Empty _ _    = []
kdtRangeSearchRecY (Node left npos ndata right) bMin bMax
        | nc < mnc
        = nextfun right bMin bMax
        
        | nc > mxc
        = nextfun left bMin bMax
        
        | (kdtInBounds npos bMin bMax)
        = (npos, ndata)
        : (nextfun right bMin bMax ++ nextfun left bMin bMax)

        | otherwise
        =  nextfun right bMin bMax ++ nextfun left bMin bMax

        where   Vec2 _ nc       = npos
                Vec2 _ mnc      = bMin
                Vec2 _ mxc      = bMax
                nextfun         = kdtRangeSearchRecX


kdtRangeSearch :: (KDTreeNode a) -> Vec2 -> Vec2 -> [(Vec2,a)]
kdtRangeSearch t bMin bMax 
        = kdtRangeSearchRecX t bMin bMax


kdtAddPoints :: [(Vec2,a)] -> (KDTreeNode a) -> (KDTreeNode a)
kdtAddPoints [] t       = t
kdtAddPoints ((pt, dat):ps) t 
        = kdtAddPoints ps $ kdtAddPoint t pt dat


singleCollision :: Vec2 -> Vec2 -> Vec2 -> Double -> a -> Maybe (Vec2, a)
singleCollision pt start a eps dat
        | sqrd_dist < eps * eps
        = Just (vecAdd start p, dat)
        
        | otherwise
        = Nothing

        where   b       = vecSub pt start
                xhat    = (vecDot a b) / (vecDot a a)
                p       = vecScale a xhat
                e       = vecSub p b
                sqrd_dist = vecDot e e


kdtCollisionDetect :: KDTreeNode a -> Vec2 -> Vec2 -> Double -> [(Vec2,a)]
kdtCollisionDetect root !start !end !eps 
 = colls 
 where   Vec2 sx sy = start
         Vec2 ex ey = end
         rmin    = Vec2 (min sx ex - eps) (min sy ey - eps)
         rmax    = Vec2 (max sx ex + eps) (max sy ey + eps)
         pts     = kdtRangeSearch root rmin rmax
         a       = vecSub end start
         colls   = mapMaybe (\(pt,dat) -> singleCollision pt start a eps dat) pts
      
      
-- Dumping --------------------------------------------------------------------
-- | Dump a KDTree to a file
dumpKDTree :: KDTreeNode Int -> FilePath -> IO ()
dumpKDTree kdt name 
 = do   h       <- openFile name WriteMode
        hPutStrLn h "n x y z"
        dumpKDTreeInner kdt h
        hClose h


-- | Dump a KDTree to a handle.
dumpKDTreeInner :: KDTreeNode Int -> Handle -> IO ()
dumpKDTreeInner kdt h 
 = case kdt of
        Empty -> return ()

        Node l v d r 
         -> do  printVec v h d
                dumpKDTreeInner l h
                dumpKDTreeInner r h


-- | Print a vector to a handle.
printVec :: Vec2 -> Handle -> Int -> IO ()
printVec (Vec2 x y) h i 
        = hPutStrLn h $ show i ++ " " ++ show x ++ " " ++ show y

