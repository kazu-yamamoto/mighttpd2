module Program.Mighty.Logger (
  -- * Types
    LogType(..)
  , ApacheLogger
  , LogFlusher
  -- * Utilities
  , initLogger
  , logCheck
  ) where

import Network.HTTP.Types
import Network.Wai
import System.Posix.IO

import Program.Mighty.Apache
import Program.Mighty.Date
import Program.Mighty.FileLog
import Program.Mighty.LogMsg

----------------------------------------------------------------

type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

type LogFlusher = IO ()

data LogType = LogNone
             | LogStdout BufSize
             | LogFile FileLogSpec BufSize

----------------------------------------------------------------

-- |
-- Creating 'ApacheLogger' according to 'LogType'.
initLogger :: IPAddrSource -> LogType -> IO (ApacheLogger, LogFlusher, DateCacheUpdater)
initLogger _     LogNone             = noLoggerInit
initLogger ipsrc (LogStdout size)    = stdoutLoggerInit ipsrc size
initLogger ipsrc (LogFile spec size) = fileLoggerInit ipsrc spec size

noLoggerInit :: IO (ApacheLogger, LogFlusher, DateCacheUpdater)
noLoggerInit = return $! (noLogger, noFlusher, noUpdater)
  where
    noLogger _ _ _ = return ()
    noFlusher = return ()
    noUpdater = return ()

stdoutLoggerInit :: IPAddrSource -> BufSize
                 -> IO (ApacheLogger, LogFlusher, DateCacheUpdater)
stdoutLoggerInit ipsrc size = do
    (dateget, updater) <- clockDateCacher zonedDateCacheConf
    lgr <- newLogger stdOutput size
    return $! (logger lgr dateget, flushLogMsg lgr, updater)
  where
    logger lgr dateget req st mlen = do
        zdata <- dateget
        pushLogMsg lgr (apacheLogMsg ipsrc zdata req st mlen)

----------------------------------------------------------------

fileLoggerInit :: IPAddrSource -> FileLogSpec -> BufSize
               -> IO (ApacheLogger, LogFlusher, DateCacheUpdater)
fileLoggerInit ipsrc spec size = do
    (dateget, updater) <- clockDateCacher zonedDateCacheConf
    fd <- logOpen (log_file spec)
    lgr <- newLogger fd size
    return $! (logger lgr dateget, flushLogMsg lgr, updater)
  where
    logger lgr dateget req st mlen = do
        zdata <- dateget
        pushLogMsg lgr (apacheLogMsg ipsrc zdata req st mlen)

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
logCheck LogNone          = return ()
logCheck (LogStdout _)    = return ()
logCheck (LogFile spec _) = check spec
