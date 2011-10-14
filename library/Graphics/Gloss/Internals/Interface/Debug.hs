{-# OPTIONS_HADDOCK hide #-}

-- | Implements functions to dump portions of the OpenGL state to stdout.
--	Used for debugging.
module Graphics.Gloss.Internals.Interface.Debug
	( dumpFramebufferState
	, dumpFragmentState )
where
import qualified Graphics.Rendering.OpenGL.GL	as GL
import Graphics.Rendering.OpenGL		(get)
		
-- | Dump internal state of the OpenGL framebuffer
dumpFramebufferState :: IO ()
dumpFramebufferState
 = do
 	auxBuffers	<- get GL.auxBuffers
	doubleBuffer	<- get GL.doubleBuffer
	drawBuffer	<- get GL.drawBuffer

	rgbaBits	<- get GL.rgbaBits
	stencilBits	<- get GL.stencilBits
	depthBits	<- get GL.depthBits
	accumBits	<- get GL.accumBits
	
	clearColor	<- get GL.clearColor
	clearStencil	<- get GL.clearStencil
	clearDepth	<- get GL.clearDepth
	clearAccum	<- get GL.clearAccum
	
	colorMask	<- get GL.colorMask
	stencilMask	<- get GL.stencilMask
	depthMask	<- get GL.depthMask
	
	putStr	$  "* dumpFramebufferState\n"
		++ "  auxBuffers         = " ++ show auxBuffers		++ "\n"
		++ "  doubleBuffer       = " ++ show doubleBuffer	++ "\n"
		++ "  drawBuffer         = " ++ show drawBuffer		++ "\n"
		++ "\n"
		++ "  bits       rgba    = " ++ show rgbaBits		++ "\n"
		++ "             stencil = " ++ show stencilBits	++ "\n"
		++ "             depth   = " ++ show depthBits		++ "\n"
		++ "             accum   = " ++ show accumBits		++ "\n"
		++ "\n"
		++ "  clear      color   = " ++ show clearColor		++ "\n"
		++ "             stencil = " ++ show clearStencil	++ "\n"
		++ "             depth   = " ++ show clearDepth		++ "\n"
		++ "             accum   = " ++ show clearAccum		++ "\n"
		++ "\n"
		++ "  mask       color   = " ++ show colorMask		++ "\n"
		++ "             stencil = " ++ show stencilMask	++ "\n"
		++ "             depth   = " ++ show depthMask		++ "\n"
		++ "\n"
		

-- | Dump internal state of the fragment renderer.
dumpFragmentState :: IO ()
dumpFragmentState
 = do
 	blend		<- get GL.blend
	blendEquation	<- get GL.blendEquation
	blendFunc	<- get GL.blendFunc
	
	putStr	$  "* dumpFragmentState\n"
		++ "  blend              = " ++ show blend		++ "\n"
		++ "  blend equation     = " ++ show blendEquation	++ "\n"
		++ "  blend func         = " ++ show blendFunc		++ "\n"
		++ "\n"
