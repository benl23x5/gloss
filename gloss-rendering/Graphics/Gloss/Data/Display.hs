
module Graphics.Gloss.Data.Display
        (Display(..))
where

-- | Describes how Gloss should display its output.
data Display
        -- | Display in a window with the given name, size and position.
        = InWindow   String (Int, Int) (Int, Int)

        -- | Display full screen with a drawing area of the given size.
        | FullScreen (Int, Int) 
        deriving (Eq, Read, Show)
