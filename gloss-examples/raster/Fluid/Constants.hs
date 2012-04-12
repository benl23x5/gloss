module Constants
        ( widthArg
        , dtArg
        , diffArg
        , viscArg
        , windowWidthArg
        , densArg
        , velArg)
where
import System.IO.Unsafe
import Data.IORef


widthArg :: IORef Int
widthArg        = unsafePerformIO $ newIORef 100
{-# NOINLINE widthArg #-}


dtArg :: IORef Float
dtArg = unsafePerformIO $ newIORef 0.1
{-# NOINLINE dtArg #-}


diffArg :: IORef Float
diffArg = unsafePerformIO $ newIORef 0
{-# NOINLINE diffArg #-}


viscArg :: IORef Float
viscArg = unsafePerformIO $ newIORef 0
{-# NOINLINE viscArg #-}


windowWidthArg :: IORef Int
windowWidthArg = unsafePerformIO $ newIORef 500
{-# NOINLINE windowWidthArg #-}


densArg :: IORef Float
densArg = unsafePerformIO $ newIORef 100
{-# NOINLINE densArg #-}


velArg :: IORef (Float, Float)
velArg = unsafePerformIO $ newIORef (20, 20)
{-# NOINLINE velArg #-}

