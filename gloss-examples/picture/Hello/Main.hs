
-- | Display "Hello World" in a window.
--
import Graphics.Gloss

main    
 = display 
        (InWindow
               "Hello World"     -- window title
                (400, 150)       -- window size
                (10, 10))        -- window position
        white                    -- background color
        picture                  -- picture to display

picture 
        = Translate (-170) (-20) -- shift the text to the middle of the window
        $ Scale 0.5 0.5          -- display it half the original size
        $ Text "Hello World"     -- text to display
