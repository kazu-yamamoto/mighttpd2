module FileCache (fileCacheInit) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.HashMap (Map)
import qualified Data.HashMap as M
import Data.IORef
import Network.HTTP.Date
import Network.Wai.Application.Classic
import System.IO.Unsafe
import System.Posix.Files

data Entry = Negative | Positive FileInfo
type Cache = Map ByteString Entry
type GetInfo = ByteString -> IO (Maybe FileInfo)

fileInfo :: IORef Cache -> GetInfo
fileInfo ref path = atomicModifyIORef ref (lok path)

lok :: ByteString -> Cache -> (Cache, Maybe FileInfo)
lok path cache = unsafePerformIO $ do
    let ment = M.lookup path cache
    case ment of
        Nothing -> handle handler $ do
            let sfile =  BS.unpack path
            fs <- getFileStatus sfile
            if doesExist fs then pos fs sfile else neg
        Just Negative     -> return (cache, Nothing)
        Just (Positive x) -> return (cache, Just x)
  where
    size = fromIntegral . fileSize
    mtime = epochTimeToHTTPDate . modificationTime
    doesExist = not . isDirectory
    pos fs sfile = do
        let info = FileInfo {
                fileInfoName = sfile
              , fileInfoSize = size fs
              , fileInfoTime = mtime fs
              }
            entry = Positive info
            cache' = M.insert path entry cache
        return (cache', Just info)
    neg = do
        let cache' = M.insert path Negative cache
        return (cache', Nothing)
    handler :: SomeException -> IO (Cache, Maybe FileInfo)
    handler _ = neg

fileCacheInit :: IO GetInfo
fileCacheInit = do
    ref <- newIORef M.empty
    forkIO (remover ref)
    return $ fileInfo ref

-- atomicModifyIORef is not necessary here.
remover :: IORef Cache -> IO ()
remover ref = forever $ threadDelay 10000000 >> writeIORef ref M.empty
