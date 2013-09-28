module Program.Mighty.Logger (
  -- * Types
    Logger
  , LogType(..)
  , LoggerRef
  -- * Accessor
  , apatcheLogger
  -- * Starting and closing
  , initLogger
  , finLogger
  -- * Utiles
--  , reopen
  , logCheck
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Array
import Data.IORef
import qualified Network.Wai.Logger as WL
import System.Date.Cache
import System.IO
import qualified System.Log.FastLogger as FL

----------------------------------------------------------------

type LogFlusher = IO ()

data Logger = Logger WL.ApacheLogger LogFlusher

data LogType = LogNone
             | LogStdout
               -- | 'Signal' is used to tell child processes to reopen a log file.
             | LogFile FL.FileLogSpec

----------------------------------------------------------------

apatcheLogger :: Logger -> WL.ApacheLogger
apatcheLogger (Logger aplogr _) = aplogr

----------------------------------------------------------------

initLogger :: WL.IPAddrSource -> LogType -> IO Logger
initLogger ipsrc logtyp = do
    (aplgr, flusher) <- logInit ipsrc logtyp
    return $ Logger aplgr flusher

finLogger :: Logger -> IO ()
finLogger (Logger _ flusher) = flusher

----------------------------------------------------------------

-- |
-- Creating 'ApacheLogger' according to 'LogType'.
logInit :: WL.IPAddrSource -> LogType -> IO (WL.ApacheLogger, LogFlusher)
logInit _     LogNone        = noLoggerInit
logInit ipsrc LogStdout      = stdoutLoggerInit ipsrc
logInit ipsrc (LogFile spec) = fileLoggerInit ipsrc spec

noLoggerInit :: IO (WL.ApacheLogger, LogFlusher)
noLoggerInit = return $! (noLogger, noFlusher)
  where
    noLogger _ _ _ = return ()
    noFlusher = return ()

stdoutLoggerInit :: WL.IPAddrSource -> IO (WL.ApacheLogger, LogFlusher)
stdoutLoggerInit ipsrc = do
    dc <- clockDateCacher FL.zonedDateCacheConf
    lgr <- WL.stdoutApacheLoggerInit2 ipsrc True dc
    return $! (lgr, return ())

----------------------------------------------------------------

type LoggerSet = Array Int FL.Logger

newtype LoggerRef = LoggerRef (IORef LoggerSet)

getLogger :: LoggerRef -> IO LoggerSet
getLogger (LoggerRef ref) = readIORef ref

setLogger :: LoggerRef -> LoggerSet -> IO ()
setLogger (LoggerRef ref) = writeIORef ref

----------------------------------------------------------------

fileLoggerInit :: WL.IPAddrSource -> FL.FileLogSpec
               -> IO (WL.ApacheLogger, LogFlusher)
fileLoggerInit ipsrc spec = do
    n <- getNumCapabilities
    hdls <- replicateM n $ open spec
    dc <- clockDateCacher FL.zonedDateCacheConf
    loggers <- mapM (\hdl -> FL.mkLogger2 False hdl dc) hdls
    let loggerset = listArray (0, n - 1) loggers
    logref <- LoggerRef <$> newIORef loggerset
    return (fileLogger ipsrc logref, fileFlusher logref)

open :: FL.FileLogSpec -> IO Handle
open spec = openFile (FL.log_file spec) AppendMode

{- FIXME
reopen :: FL.FileLogSpec -> LoggerRef -> IO ()
reopen spec logref = do
    oldlogger <- getLogger logref
    newlogger <- open spec >>= FL.renewLogger oldlogger
    setLogger logref newlogger
-}

----------------------------------------------------------------

fileLogger :: WL.IPAddrSource -> LoggerRef -> WL.ApacheLogger
fileLogger ipsrc logref req status msiz = do
    loggerset <- getLogger logref
    (i,_) <- myThreadId >>= threadCapability
    let logger = loggerset ! i
    date <- FL.loggerDate logger
    FL.loggerPutStr logger $ WL.apacheFormat ipsrc date req status msiz

fileFlusher :: LoggerRef -> IO ()
fileFlusher logref = do
    loggerset <- getLogger logref
    mapM_ FL.loggerFlush $ elems loggerset

----------------------------------------------------------------

{-
-- FIXME
fileLoggerController :: FL.FileLogSpec -> IO ()
fileLoggerController spec = forever $ do
    isOver <- over
    when isOver $ FL.rotate spec
    threadDelay 10000000
  where
    file = FL.log_file spec
    over = handle handler $ do
        siz <- fromIntegral . fileSize <$> getFileStatus file
        if siz > FL.log_file_size spec then
            return True
          else
            return False
    handler :: SomeException -> IO Bool
    handler _ = return False
-}

----------------------------------------------------------------

-- |
-- Checking if a log file can be written if 'LogType' is 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone        = return ()
logCheck LogStdout      = return ()
logCheck (LogFile spec) = FL.check spec
