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
  -- * Log controller
  , fileLoggerController
  -- * Utiles
  , reopen
  , logCheck
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

newtype LoggerRef = LoggerRef (IORef FL.Logger)

getLogger :: LoggerRef -> IO FL.Logger
getLogger (LoggerRef ref) = readIORef ref

setLogger :: LoggerRef -> FL.Logger -> IO ()
setLogger (LoggerRef ref) = writeIORef ref

----------------------------------------------------------------

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


----------------------------------------------------------------

-- |
-- Checking if a log file can be written if 'LogType' is 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone        = return ()
logCheck LogStdout      = return ()
logCheck (LogFile spec) = FL.check spec
