module Constants
        ( widthI
        , widthF
        , dt
        , diff
        , windowWidth
        , windowHeight
        , scaleX
        , scaleY
        , visc
        , newDensity
        , newVelocity

        -- Arguments: defined as IORefs
        , widthArg
        , dtArg
        , diffArg
        , viscArg
        , windowWidthArg
        , densArg
        , velArg)
where

import System.IO.Unsafe
import Data.IORef

-- Width ----------------------------------------------------------------------
widthArg :: IORef Int
widthArg        = unsafePerformIO $ newIORef 100
{-# NOINLINE widthArg #-}

widthI :: Int
widthI = unsafePerformIO $ readIORef widthArg
{-# INLINE widthI #-}

widthF :: Float
widthF = fromIntegral widthI
{-# INLINE widthF #-}


-- dt -------------------------------------------------------------------------
dtArg :: IORef Float
dtArg = unsafePerformIO $ newIORef 0.1
{-# NOINLINE dtArg #-}

dt :: Float
dt = unsafePerformIO $ readIORef dtArg
{-# INLINE dt #-}


-- diff -----------------------------------------------------------------------
diffArg :: IORef Float
diffArg = unsafePerformIO $ newIORef 0
{-# NOINLINE diffArg #-}

diff :: Float
diff = unsafePerformIO $ readIORef diffArg
{-# INLINE diff #-}


-- visc -----------------------------------------------------------------------
viscArg :: IORef Float
viscArg = unsafePerformIO $ newIORef 0
{-# NOINLINE viscArg #-}

visc :: Float
visc = unsafePerformIO $ readIORef viscArg
{-# INLINE visc #-}


-- window ---------------------------------------------------------------------
windowWidthArg :: IORef Int
windowWidthArg = unsafePerformIO $ newIORef 500
{-# NOINLINE windowWidthArg #-}

windowWidth :: Int
windowWidth = unsafePerformIO $ readIORef windowWidthArg
{-# INLINE windowWidth #-}

windowHeight :: Int
windowHeight = windowWidth
{-# INLINE windowHeight #-}


-- scale ----------------------------------------------------------------------
scaleX :: Float
scaleX = fromIntegral (windowWidth `div` widthI)
{-# INLINE scaleX #-}

scaleY :: Float
scaleY = scaleX
{-# INLINE scaleY #-}


-- density --------------------------------------------------------------------
densArg :: IORef Float
densArg = unsafePerformIO $ newIORef 100
{-# NOINLINE densArg #-}

newDensity :: Float
newDensity = unsafePerformIO $ readIORef densArg
{-# INLINE newDensity #-}


-- velocity -------------------------------------------------------------------
velArg :: IORef (Float, Float)
velArg = unsafePerformIO $ newIORef (20, 20)
{-# NOINLINE velArg #-}

newVelocity :: (Float, Float)
newVelocity = unsafePerformIO $ readIORef velArg
{-# INLINE newVelocity #-}

