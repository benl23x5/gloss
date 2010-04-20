module Config where


simPeriod	= 0.1	:: Float
worldHeight	= 100	:: Int
worldWidth	= 100	:: Int
cellSize	= 5     :: Int
cellSpace	= 1	:: Int
windowHeight	= (cellSize + cellSpace) * worldHeight + cellSpace
windowWidth	= (cellSize + cellSpace) * worldWidth  + cellSpace
