{-# LANGUAGE BangPatterns #-}

module Utils where

import Control.Exception
import Data.IORef

----------------------------------------------------------------

ignore :: SomeException -> IO ()
ignore _ = return ()

printStdout :: SomeException -> IO ()
printStdout = print

----------------------------------------------------------------

strictAtomicModifyIORef :: IORef a -> (a -> a) -> IO ()
strictAtomicModifyIORef ref f = do
    !_ <- atomicModifyIORef ref (\x -> let !r = f x in (r, ()))
    return ()

----------------------------------------------------------------

infixr 0 >>>=

(>>>=) :: IO (Maybe a) -> (a -> IO ()) -> IO ()
x >>>= f = x >>= maybe (return ()) f
