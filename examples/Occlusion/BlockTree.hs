
module BlockTree
where
import Extent	
	

data BlockNode a
	-- | An empty node in the tree. 
	--	It contains no cells.
	= TNil

	-- | A single active block in the tree.
	--	May appear at any depth.
	| TBlock a
	
	-- | A node with four children.
	| TNode (BlockNode a) (BlockNode a) 	-- NW NE
		(BlockNode a) (BlockNode a)	-- SW SE
		
	deriving (Show, Eq)

makeNode :: BlockNode a -> BlockNode a
makeNode tree
 = case tree of
	TNil	-> TNode TNil TNil TNil TNil
	TNode{}	-> tree
			

data BlockTree a
	= BlockTree
	{ treeExtent	:: Int
	, treeNodes	:: BlockNode a }


insert 	:: Int		-- ^ x position 
	-> Int		-- ^ y posision
	-> a		-- ^ value to insert
	-> Tree		-- ^ old tree
	-> Tree
	
insert pos block tree
 = insert' 0 (treeExtent tree) (treeNodes tree)

 where	insert' depth extent
		| depth == maxDepth
		= TBlock block

		| otherwise	
		= foldr intoQuad tree [0..3]

	intoQuad depth extent quad node
		| extent'	<- cutQuadExtent quad node
		, isInExtent extent' pos
		= appQuad quad (insert' (depth + 1) extent') node
		
		| otherwise
		= node
		

appQuad :: Int -> (BlockNode a -> BlockNode a) -> BlockNode a -> BlockNode a
appQuad quad f node
 = let	TNode xnw xne xsw xse 	= makeNode node
   in	case quad of
	 0	-> TNode (f xnw) xne     xsw     xse
	 1	-> TNode xnw     (f xne) xsw     xse
	 2	-> TNode xnw     xne     (f xsw) xse
	 3	-> TNode xnw     xne     xsw     (f xse)
