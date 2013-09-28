module Program.Mighty.Logger (
  -- * Types
    Logger
  , LogType(..)
  , LogController
  , LoggerRef
  , LogFlusher
  -- * Starting and closing
  , initLogger
  , finLogger
  -- * Utiles
  , apatcheLogger
  , reopen
  , logCheck
  -- * Log controller
  , logController
  , noLoggerController
  , fileLoggerController
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception (handle, SomeException)
import Control.Monad
import Data.IORef
import qualified Network.Wai.Logger as WL
import System.Date.Cache
import System.IO
import qualified System.Log.FastLogger as FL
import System.Posix

----------------------------------------------------------------

data Logger = Logger WL.ApacheLogger LogFlusher

initLogger :: WL.IPAddrSource -> LogType -> IO Logger
initLogger ipsrc logtyp = do
    (aplgr, flusher) <- logInit ipsrc logtyp
    return $ Logger aplgr flusher

finLogger :: Logger -> LogFlusher
finLogger (Logger _ flusher) = flusher

apatcheLogger :: Logger -> WL.ApacheLogger
apatcheLogger (Logger aplogr _) = aplogr

----------------------------------------------------------------

-- |
-- Checking if a log file can be written if 'LogType' is 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone        = return ()
logCheck LogStdout      = return ()
logCheck (LogFile spec) = FL.check spec

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

-- |
-- Creating a log controller against child processes.
logController :: LogType -> LogController
logController LogNone        = noLoggerController
logController LogStdout      = noLoggerController
logController (LogFile spec) = fileLoggerController spec

noLoggerController :: LogController
noLoggerController = forever $ threadDelay maxBound

----------------------------------------------------------------

newtype LoggerRef = LoggerRef (IORef FL.Logger)

getLogger :: LoggerRef -> IO FL.Logger
getLogger (LoggerRef ref) = readIORef ref

setLogger :: LoggerRef -> FL.Logger -> IO ()
setLogger (LoggerRef ref) = writeIORef ref

----------------------------------------------------------------

type LogFlusher = IO ()

fileLoggerInit :: WL.IPAddrSource -> FL.FileLogSpec
               -> IO (WL.ApacheLogger, LogFlusher)
fileLoggerInit ipsrc spec = do
    hdl <- open spec
    dc <- clockDateCacher FL.zonedDateCacheConf
    logger <- FL.mkLogger2 False hdl dc
    logref <- LoggerRef <$> newIORef logger
    void . forkIO $ fileFlusher logref
    return (fileLogger ipsrc logref, fileFlusher' logref)

open :: FL.FileLogSpec -> IO Handle
open spec = openFile (FL.log_file spec) AppendMode

reopen :: FL.FileLogSpec -> LoggerRef -> IO ()
reopen spec logref = do
    oldlogger <- getLogger logref
    newlogger <- open spec >>= FL.renewLogger oldlogger
    setLogger logref newlogger

----------------------------------------------------------------

fileLogger :: WL.IPAddrSource -> LoggerRef -> WL.ApacheLogger
fileLogger ipsrc logref req status msiz = do
    logger <- getLogger logref
    date <- FL.loggerDate logger
    FL.loggerPutStr logger $ WL.apacheFormat ipsrc date req status msiz

fileFlusher :: LoggerRef -> IO ()
fileFlusher logref = forever $ do
    threadDelay 10000000
    fileFlusher' logref

fileFlusher' :: LoggerRef -> IO ()
fileFlusher' logref = getLogger logref >>= FL.loggerFlush

----------------------------------------------------------------

fileLoggerController :: FL.FileLogSpec -> LogController
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

----------------------------------------------------------------

data LogType = LogNone
             | LogStdout
               -- | 'Signal' is used to tell child processes to reopen a log file.
             | LogFile FL.FileLogSpec

type LogController = IO ()
