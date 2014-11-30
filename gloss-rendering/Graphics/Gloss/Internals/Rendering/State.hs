{-# OPTIONS_HADDOCK hide #-}

-- | Rendering options
module Graphics.Gloss.Internals.Rendering.State
	( State (..)
	, initState
	, Texture (..))
where
import Graphics.Gloss.Internals.Data.Picture
import Foreign.ForeignPtr
import System.Mem.StableName
import Data.Word
import Data.IORef
import qualified Graphics.Rendering.OpenGL.GL   as GL


-- | Render options settings
data State
	= State
	{ -- | Whether to use color
	  stateColor		:: !Bool

	-- | Whether to force wireframe mode only
	, stateWireframe	:: !Bool

	-- | Whether to use alpha blending
	, stateBlendAlpha	:: !Bool

	-- | Whether to use line smoothing
	, stateLineSmooth	:: !Bool
	
	-- | Cache of Textures that we've sent to OpenGL.
	, stateTextures         :: !(IORef [Texture])
	}
	

-- | A texture that we've sent to OpenGL.
data Texture
        = Texture
        { -- | Stable name derived from the `BitmapData` that the user gives us.
          texName       :: StableName BitmapData

        -- | Width of the image, in pixels.
        , texWidth      :: Int

        -- | Height of the image, in pixels.
        , texHeight     :: Int

        -- | Pointer to the Raw texture data.
        , texData       :: ForeignPtr Word8
        
        -- | The OpenGL texture object.
        , texObject     :: GL.TextureObject

        -- | Whether we want to leave this in OpenGL texture memory between frames.
        , texCacheMe    :: Bool }


-- | A mutable render state holds references to the textures currently loaded
--   into the OpenGL context. To ensure that textures are cached in GPU memory,
--   pass the same `State` each time you call `displayPicture` or `renderPicture`.
initState :: IO State
initState
 = do   textures        <- newIORef []
	return  State
	        { stateColor		= True
                , stateWireframe	= False
	        , stateBlendAlpha	= True
	        , stateLineSmooth	= False 
	        , stateTextures         = textures }
	

