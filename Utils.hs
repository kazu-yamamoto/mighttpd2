{-# LANGUAGE BangPatterns #-}

module Utils where

import Control.Exception
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO

----------------------------------------------------------------

ignore :: SomeException -> IO ()
ignore _ = return ()

printStdout :: SomeException -> IO ()
printStdout x = print x >> hFlush stdout

----------------------------------------------------------------

strictAtomicModifyIORef :: IORef a -> (a -> a) -> IO ()
strictAtomicModifyIORef ref f = do
    !_ <- atomicModifyIORef ref (\x -> let !r = f x in (r, ()))
    return ()

----------------------------------------------------------------

bshow :: Show a => a -> ByteString
bshow = BS.pack . show

infixr 5 +++

(+++) :: ByteString -> ByteString -> ByteString
(+++) = BS.append
    
----------------------------------------------------------------

infixr 0 >>>=

(>>>=) :: IO (Maybe a) -> (a -> IO ()) -> IO ()
x >>>= f = x >>= maybe (return ()) f
