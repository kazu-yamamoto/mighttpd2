{-# LANGUAGE RecordWildCards #-}

module Program.Mighty.FileCache (
  -- * Types
    GetInfo
  , RemoveInfo
  -- * Starter
  , fileCacheInit
  ) where

import Control.Exception
import Control.Exception.IOChoice
import Control.Reaper
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Network.HTTP.Date
import Network.Wai.Application.Classic
import System.Posix.Files

data Entry = Negative | Positive FileInfo
type Cache = HashMap ByteString Entry
type GetInfo = Path -> IO FileInfo
type RemoveInfo = IO ()
type FileCache = Reaper Cache (ByteString,Entry)

fileInfo :: FileCache -> GetInfo
fileInfo reaper@Reaper{..} path = do
    cache <- reaperRead
    case M.lookup bpath cache of
        Just Negative     -> throwIO (userError "fileInfo")
        Just (Positive x) -> return x
        Nothing           -> register ||> negative reaper path
  where
    bpath = pathByteString path
    sfile = pathString path
    register = do
        fs <- getFileStatus sfile
        let regular = not (isDirectory fs)
            readable = fileMode fs `intersectFileModes` ownerReadMode /= 0
        if regular && readable then
            positive reaper fs path
          else
            goNext

positive :: FileCache -> FileStatus -> GetInfo
positive Reaper{..} fs path = do
    reaperAdd (bpath,entry)
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

negative :: FileCache -> GetInfo
negative Reaper{..} path = do
    reaperAdd (bpath,Negative)
    throwIO (userError "fileInfo")
  where
    bpath = pathByteString path

----------------------------------------------------------------

fileCacheInit :: IO GetInfo
fileCacheInit = mkReaper settings >>= return . fileInfo
  where
    settings = defaultReaperSettings {
        reaperAction = override
      , reaperDelay  = 10000000 -- 10 seconds
      , reaperCons   = uncurry M.insert
      , reaperNull   = M.null
      , reaperEmpty  = M.empty
      }

override :: Cache -> IO (Cache -> Cache)
override _ = return $ const M.empty
