
module Graphics.Gloss.Data.Quad
        ( Quad(..)
        , allQuads)
where

-- | Represents a Quadrant in the 2D plane.
data Quad
        = NW    -- ^ North West
        | NE    -- ^ North East
        | SW    -- ^ South West
        | SE    -- ^ South East
        deriving (Show, Eq, Enum)

-- | A list of all quadrants. Same as @[NW .. SE]@.
allQuads :: [Quad]
allQuads = [NW .. SE]
