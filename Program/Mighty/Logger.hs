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
import Data.IORef
import Network.HTTP.Types
import Network.Wai
import System.IO

import Program.Mighty.Apache
import Program.Mighty.Date
import Program.Mighty.FileLog

----------------------------------------------------------------

type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

type LogFlusher = IO ()

data Logger = Logger ApacheLogger LogFlusher

data LogType = LogNone
             | LogStdout
               -- | 'Signal' is used to tell child processes to reopen a log file.
             | LogFile FileLogSpec

----------------------------------------------------------------

apatcheLogger :: Logger -> ApacheLogger
apatcheLogger (Logger aplogr _) = aplogr

----------------------------------------------------------------

initLogger :: IPAddrSource -> LogType -> IO Logger
initLogger ipsrc logtyp = do
    (aplgr, flusher) <- logInit ipsrc logtyp
    return $ Logger aplgr flusher

finLogger :: Logger -> IO ()
finLogger (Logger _ flusher) = flusher

----------------------------------------------------------------

-- |
-- Creating 'ApacheLogger' according to 'LogType'.
logInit :: IPAddrSource -> LogType -> IO (ApacheLogger, LogFlusher)
logInit _     LogNone        = noLoggerInit
logInit ipsrc LogStdout      = stdoutLoggerInit ipsrc
logInit ipsrc (LogFile spec) = fileLoggerInit ipsrc spec

noLoggerInit :: IO (ApacheLogger, LogFlusher)
noLoggerInit = return $! (noLogger, noFlusher)
  where
    noLogger _ _ _ = return ()
    noFlusher = return ()

stdoutLoggerInit :: IPAddrSource -> IO (ApacheLogger, LogFlusher)
stdoutLoggerInit ipsrc = do
    dc <- clockDateCacher zonedDateCacheConf
    lgr <- undefined
    return $! (lgr, return ())

----------------------------------------------------------------

newtype LoggerRef = LoggerRef (IORef Logger)

getLogger :: LoggerRef -> IO Logger
getLogger (LoggerRef ref) = readIORef ref

setLogger :: LoggerRef -> Logger -> IO ()
setLogger (LoggerRef ref) = writeIORef ref

----------------------------------------------------------------

fileLoggerInit :: IPAddrSource -> FileLogSpec
               -> IO (ApacheLogger, LogFlusher)
fileLoggerInit ipsrc spec = do
    n <- getNumCapabilities
    hdls <- replicateM n $ open spec
    dc <- clockDateCacher zonedDateCacheConf
    let logger = undefined
    logref <- LoggerRef <$> newIORef logger
    return (fileLogger ipsrc logref, fileFlusher logref)

open :: FileLogSpec -> IO Handle
open spec = openFile (log_file spec) AppendMode

{- FIXME
reopen :: FileLogSpec -> LoggerRef -> IO ()
reopen spec logref = do
    oldlogger <- getLogger logref
    newlogger <- open spec >>= renewLogger oldlogger
    setLogger logref newlogger
-}

----------------------------------------------------------------

fileLogger :: IPAddrSource -> LoggerRef -> ApacheLogger
fileLogger ipsrc logref req status msiz = do
    date <- undefined
    undefined $ apacheLogMsg ipsrc date req status msiz

fileFlusher :: LoggerRef -> IO ()
fileFlusher logref = do
    loggerset <- getLogger logref
    undefined

----------------------------------------------------------------

{-
-- FIXME
fileLoggerController :: FileLogSpec -> IO ()
fileLoggerController spec = forever $ do
    isOver <- over
    when isOver $ rotate spec
    threadDelay 10000000
  where
    file = log_file spec
    over = handle handler $ do
        siz <- fromIntegral . fileSize <$> getFileStatus file
        if siz > log_file_size spec then
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
logCheck (LogFile spec) = check spec
