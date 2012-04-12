module Constants
        ( widthArg
        , dt
        , diff
        , visc
        , newDensity
        , newVelocity

        -- Arguments: defined as IORefs
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

