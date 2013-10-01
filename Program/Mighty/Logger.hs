module Program.Mighty.Logger (
  -- * Types
    LogType(..)
  , ApacheLogger
  , LogFlusher
  -- * Utilities
  , initLogger
  , logCheck
  ) where

import Control.Applicative ((<$>))
import Control.Exception (handle,SomeException(..))
import Control.Monad (when)
import Network.HTTP.Types
import Network.Wai
import System.Posix.IO
import System.Posix.Types (Fd)
import System.Posix.Files (getFileStatus, fileSize)

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
initLogger :: IPAddrSource -> LogType -> DateCacheGetter -> IO (ApacheLogger, LogFlusher)
initLogger _     LogNone             _       = noLoggerInit
initLogger ipsrc (LogStdout size)    dateget = stdoutLoggerInit ipsrc size dateget
initLogger ipsrc (LogFile spec size) dateget = fileLoggerInit ipsrc spec size dateget

----------------------------------------------------------------

noLoggerInit :: IO (ApacheLogger, LogFlusher)
noLoggerInit = return $! (noLogger, noFlusher)
  where
    noLogger _ _ _ = return ()
    noFlusher = return ()

stdoutLoggerInit :: IPAddrSource -> BufSize -> DateCacheGetter
                 -> IO (ApacheLogger, LogFlusher)
stdoutLoggerInit ipsrc size dateget = loggerInit ipsrc size dateget stdOutput

fileLoggerInit :: IPAddrSource -> FileLogSpec -> BufSize -> DateCacheGetter
               -> IO (ApacheLogger, LogFlusher)
fileLoggerInit ipsrc spec size dateget =
    logOpen (log_file spec) >>= loggerInit ipsrc size dateget

----------------------------------------------------------------

loggerInit :: IPAddrSource -> BufSize -> DateCacheGetter -> Fd
           -> IO (ApacheLogger, LogFlusher)
loggerInit ipsrc size dateget fd = do
    lgrset <- newLoggerSet fd size
    return $! (apache lgrset ipsrc dateget, flushLogMsg lgrset)

apache :: LoggerSet -> IPAddrSource -> DateCacheGetter -> ApacheLogger
apache lgrset ipsrc dateget req st mlen = do
    zdata <- dateget
    pushLogMsg lgrset (apacheLogMsg ipsrc zdata req st mlen)

----------------------------------------------------------------

rotater :: LoggerSet -> FileLogSpec -> IO ()
rotater lgrset spec = do
    over <- isOver
    when over $ do
        rotate spec
        logOpen (log_file spec) >>= renewLoggerSet lgrset
  where
    file = log_file spec
    isOver = handle (\(SomeException _) -> return False) $ do
        siz <- fromIntegral . fileSize <$> getFileStatus file
        if siz > log_file_size spec then
            return True
          else
            return False

----------------------------------------------------------------

-- |
-- Checking if a log file can be written if 'LogType' is 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone          = return ()
logCheck (LogStdout _)    = return ()
logCheck (LogFile spec _) = check spec
