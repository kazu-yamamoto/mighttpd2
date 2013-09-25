{-# LANGUAGE BangPatterns #-}

module Utils where

import Data.IORef

----------------------------------------------------------------

strictAtomicModifyIORef :: IORef a -> (a -> a) -> IO ()
strictAtomicModifyIORef ref f = do
    !_ <- atomicModifyIORef ref (\x -> let !r = f x in (r, ()))
    return ()
