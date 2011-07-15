{-# LANGUAGE OverloadedStrings #-}

module Log (
    logInit
  , logController
  , logCheck
  , module Log.Types
  ) where

import Control.Concurrent
import Control.Monad
import Log.Check
import Log.File
import Log.Stdout
import Log.Types

----------------------------------------------------------------

logInit :: LogType -> IO Logger
logInit LogNone        = noLoggerInit
logInit LogStdout      = stdoutLoggerInit
logInit (LogFile spec) = fileLoggerInit spec

noLoggerInit :: IO Logger
noLoggerInit = return noLogger
  where
    noLogger _ _ _ = return ()

logController :: LogType -> LogController
logController LogNone        = noLoggerController
logController LogStdout      = noLoggerController
logController (LogFile spec) = fileLoggerController spec

noLoggerController :: LogController
noLoggerController _ = forever $ threadDelay 10000000
