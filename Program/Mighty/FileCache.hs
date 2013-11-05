module Program.Mighty.FileCache (
  -- * Types
    GetInfo
  , RemoveInfo
  -- * Starter
  , fileCacheInit
  ) where

import Control.Exception
import Control.Exception.IOChoice
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.IORef
import Network.HTTP.Date
import Network.Wai.Application.Classic
import Program.Mighty.IORef
import System.Posix.Files

data Entry = Negative | Positive FileInfo
type Cache = HashMap ByteString Entry
type GetInfo = Path -> IO FileInfo
type RemoveInfo = IO ()

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
        let regular = not (isDirectory fs)
            readable = fileMode fs `intersectFileModes` ownerReadMode /= 0
        if regular && readable then
            positive ref fs path
          else
            goNext

positive :: IORef Cache -> FileStatus -> GetInfo
positive ref fs path = do
    strictAtomicModifyIORef ref $ M.insert bpath entry
    return info
  where
    info = FileInfo {
        fileInfoName = path
      , fileInfoSize = size fs
      , fileInfoTime = time
      , fileInfoDate = formatHTTPDate time
      }
    size = fromIntegral . fileSize
    time = epochTimeToHTTPDate (modificationTime fs)
    entry = Positive info
    bpath = pathByteString path

negative :: IORef Cache -> GetInfo
negative ref path = do
    strictAtomicModifyIORef ref $ M.insert bpath Negative
    throwIO (userError "fileInfo")
  where
    bpath = pathByteString path

----------------------------------------------------------------

fileCacheInit :: IO (GetInfo, RemoveInfo)
fileCacheInit = do
    ref <- newIORef M.empty
    return (fileInfo ref, remover ref)

-- atomicModifyIORef is not necessary here.
remover :: IORef Cache -> IO ()
remover ref = writeIORef ref M.empty
