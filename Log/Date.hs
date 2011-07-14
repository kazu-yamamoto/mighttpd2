module Log.Date (dateInit, getDate, DateRef) where

import Control.Applicative
import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Time
import System.Locale

newtype DateRef = DateRef (IORef ByteString)

getDate :: DateRef -> IO ByteString
getDate (DateRef ref) = readIORef ref

dateInit :: IO DateRef
dateInit = do
    ref <- formatDate >>= newIORef
    let dateref = DateRef ref
    forkIO $ date dateref
    return dateref

date :: DateRef -> IO ()
date dateref@(DateRef ref) = do
    tmstr <- formatDate
    atomicModifyIORef ref (\_ -> (tmstr, ()))
    threadDelay 1000000
    date dateref

formatDate :: IO ByteString
formatDate =
    BS.pack . formatTime defaultTimeLocale "%d/%b/%Y:%T %z" <$> getZonedTime
