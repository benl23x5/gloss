{-# LANGUAGE PatternGuards #-}

-- The world contains a map of all the actors, along with the current
--      quadtree so we can also draw it on the screen.
module World where

import QuadTree
import Actor
import Config

import qualified Data.Map       as Map
import Data.Map                 (Map)

-- The world ------------------------------------------------------------------
data World      
        = World (Map Index Actor)       -- actors
                (QuadTree Actor)        -- tree

-- | The initial world
worldInit :: World
worldInit       
        = World actorMapInit treeInit

actorMapInit    
        = Map.fromList 
        $ map (\a -> (actorIx a, a))
        $ (walls ++ beads)

treeInit = treeZero treeSize


-- Walls ------------------
walls :: [Actor]
walls   = zipWith actorSetIndex (box ++ splitter) [10000 ..]

box :: [Actor]
box
 = let  bs      = boxSize
   in   [ Wall  0 (- bs, -bs) (bs, -bs)         -- bot
        , Wall  0 (- bs,  bs) (bs,  bs)         -- top

        , Wall  0 (- bs, -bs) (-bs, bs)         -- left
        , Wall  0 (  bs, -bs) ( bs, bs)]        -- right

splitter :: [Actor]
splitter
 =      [ Wall  0 (-15, -100) (-200, 0) 
        , Wall  0 ( 15, -100) ( 200, 0) ]


-- Beads ------------------
beads :: [Actor]
beads   
 = let  -- beads start off with their index just set to 0
        beads_raw
                = [Bead 0 0 beadRadius (beadPos ix iy) (0, 0)
                        | ix <- [0 .. beadCountX - 1]
                        , iy <- [0 .. beadCountY - 1 ] ]
        
        -- set the unique index on the beads before returning them
   in   zipWith actorSetIndex beads_raw [0..]
                         
beadPos ix iy   
 =      ( (ix * beadBoxSize) - (beadBoxSize * beadCountX / 2)
        , (iy * beadBoxSize)  )


-- QuadTree -------------------------------------------------------------------

-- | insert an actor into the tree
insertActor :: Actor -> QuadTree Actor -> QuadTree Actor

insertActor actor tree
        -- insert a bead into the tree
        | bead@(Bead ix _ radius pos@(x, y) vel)        <- actor
        = let
                -- the bottom left and top right of the bead's bounding box.
                p0      = (x - radius, y - radius)
                p1      = (x + radius, y + radius)

          in    treeInsert treeMaxDepth 0 p0 p1 bead tree

        | wall@(Wall ix (x0, y0) (x1, y1))              <- actor
        = let
                -- the bottom left and top right of the wall's bounding box.
                p0      = (min x0 x1, min y0 y1)
                p1      = (max x0 x1, max y0 y1)
        
          in    treeInsert treeMaxDepth 0 p0 p1 wall tree
        
