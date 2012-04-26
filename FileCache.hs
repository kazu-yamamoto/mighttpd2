{-# LANGUAGE BangPatterns #-}

module FileCache (fileCacheInit) where

import Control.Concurrent
import Control.Exception
import Control.Exception.IOChoice
import Control.Monad
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.IORef
import Network.HTTP.Date
import Network.Wai.Application.Classic
import System.Posix.Files

data Entry = Negative | Positive FileInfo
type Cache = HashMap ByteString Entry
type GetInfo = Path -> IO FileInfo

fileInfo :: IORef Cache -> GetInfo
fileInfo ref path = do
    cache <- readIORef ref
    case M.lookup bpath cache of
        Just Negative     -> throwIO (userError "fileInfo")
        Just (Positive x) -> return x
        Nothing           -> register ||> negative ref path
  where
    bpath = pathByteString path
    sfile = pathString path
    register = do
        fs <- getFileStatus sfile
        if not (isDirectory fs) then
            positive ref fs path
          else
            goNext

positive :: IORef Cache -> FileStatus -> GetInfo
positive ref fs path = do
    !_ <- atomicModifyIORef ref modify
    return info
  where
    info = FileInfo {
        fileInfoName = path
        , fileInfoSize = size fs
        , fileInfoTime = mtime fs
        }
    size = fromIntegral . fileSize
    mtime = epochTimeToHTTPDate . modificationTime
    entry = Positive info
    bpath = pathByteString path
    modify cache = (cache', ())
      where
        cache' = M.insert bpath entry cache

negative :: IORef Cache -> GetInfo
negative ref path = do
    !_ <- atomicModifyIORef ref modify
    throwIO (userError "fileInfo")
  where
    bpath = pathByteString path
    modify cache = (cache', ())
      where
        cache' = M.insert bpath Negative cache

----------------------------------------------------------------

fileCacheInit :: IO GetInfo
fileCacheInit = do
    ref <- newIORef M.empty
    forkIO (remover ref)
    return $ fileInfo ref

-- atomicModifyIORef is not necessary here.
remover :: IORef Cache -> IO ()
remover ref = forever $ threadDelay 10000000 >> writeIORef ref M.empty
