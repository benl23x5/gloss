{-# OPTIONS_HADDOCK hide #-}

-- | Rendering options
module Graphics.Gloss.Internals.Render.State
	( State (..)
	, stateInit
	, Texture (..))
where
import qualified Graphics.Rendering.OpenGL.GL	as GL
import Foreign.Ptr
import System.Mem.StableName
import Data.ByteString                          (ByteString)
import Data.Word
import Data.IORef

-- | Render options settings
data State
	= State
	{ -- | Whether to use color
	  stateColor		:: Bool

	-- | Whether to force wireframe mode only
	, stateWireframe	:: Bool

	-- | Whether to use alpha blending
	, stateBlendAlpha	:: Bool

	-- | Whether to use line smoothing
	, stateLineSmooth	:: Bool
	
	-- | Cache of Textures that we've sent to OpenGL.
	, stateTextures         :: IORef [Texture]
	}
	

-- | A texture that we've sent to OpenGL.
data Texture
        = Texture
        { -- | Stable name derived from the `ByteString` that the user gives us.
          --   We 
          texName       :: StableName ByteString

        -- | Width of the image, in pixels.
        , texWidth      :: Int

        -- | Height of the image, in pixels.
        , texHeight     :: Int

        -- | Pointer to the Raw texture data.
        , texData       :: Ptr Word8
        
        -- | The OpenGL texture object.
        , texObject     :: GL.TextureObject

        -- | Whether we want to leave this in OpenGL texture memory between frames.
        , texCacheMe    :: Bool }


-- | Default render options
stateInit :: IO State
stateInit
 = do   textures        <- newIORef []
	return  State
	        { stateColor		= True
                , stateWireframe	= False
	        , stateBlendAlpha	= True
	        , stateLineSmooth	= False 
	        , stateTextures         = textures }
	

