
module QuadTree
	( Quad (..)
	, allQuads
	, QuadTree (..)
	, emptyTree
	, emptyNode
	, takeQuad
	, liftToQuad
	, insertNodeByPath
	, lookupNodeByPath
	, lookupElemByPath)
where

data Quad
	= NW | NE | SW | SE
	deriving (Show, Eq, Enum)

allQuads
	= [NW .. SE]

data QuadTree a
	-- | An empty node in the tree. 
	--	It contains no cells.
	= TNil

	-- | A single active block in the tree.
	--	May appear at any depth.
	| TLeaf a
	
	-- | A node with four children.
	| TNode (QuadTree a) (QuadTree a) 	-- NW NE
		(QuadTree a) (QuadTree a)	-- SW SE
	deriving Show


-- | A nil tree
emptyTree = TNil


-- | A Node with just TNills for the children.
emptyNode :: QuadTree a
emptyNode = TNode TNil TNil TNil TNil


-- | Get a quadrant from a node.
--   If the tree does not have an outer node then returns `Nothing`.
takeQuad
	:: Quad
	-> QuadTree a
	-> Maybe (QuadTree a)

takeQuad quad tree
 = case tree of
	TNil		-> Nothing
	TLeaf{}		-> Nothing
	TNode nw ne sw se
	 -> case quad of
		NW	-> Just nw
		NE	-> Just ne
		SW	-> Just sw
		SE	-> Just se


-- | Apply a function to a quadrant of a node.
--   If the tree does not have an outer node then returns the original tree.
liftToQuad
	:: Quad
	-> (QuadTree a -> QuadTree a) 
	-> QuadTree a  -> QuadTree a

liftToQuad quad f tree
 = case tree of
	TNil		-> tree
	TLeaf{}		-> tree
	TNode nw ne sw se
	 -> case quad of
		NW	-> TNode (f nw) ne sw se
		NE	-> TNode nw (f ne) sw se
		SW	-> TNode nw ne (f sw) se
		SE	-> TNode nw ne sw (f se)
		 
		
-- | Insert a node into the tree at the position given by a path.
--   If the path intersects an existing TLeaf then returns the original tree.
insertNodeByPath :: [Quad] -> a -> QuadTree a -> QuadTree a
	
insertNodeByPath [] x tree
	= TLeaf x
	
insertNodeByPath (q:qs) x tree
 = case tree of
	TNil	-> liftToQuad q (insertNodeByPath qs x) emptyNode
	TLeaf{}	-> tree
	TNode{}	-> liftToQuad q (insertNodeByPath qs x) tree


-- | Lookup a node of the tree based on a given path to it.
lookupNodeByPath
	:: [Quad]
	-> QuadTree a
	-> Maybe (QuadTree a)

lookupNodeByPath [] tree
	= Just tree
	
lookupNodeByPath (q:qs) tree
 = case tree of
	TNil	-> Nothing
	TLeaf{}	-> Nothing
	TNode{}
	 -> let Just quad	= takeQuad q tree
	    in  lookupNodeByPath qs quad


-- | Lookup a node based given a path to it.
lookupElemByPath
	:: [Quad]
	-> QuadTree a
	-> Maybe a

lookupElemByPath path tree
 = case lookupNodeByPath path tree of
	Just (TLeaf x)	-> Just x
	_		-> Nothing
	


