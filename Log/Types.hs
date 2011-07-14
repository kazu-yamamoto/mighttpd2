module Log.Types (
    Logger
  , FileLogSpec(..)
  , LogType(..)
  , LogController
  , LogStr(..)
  ) where

import Data.ByteString
import Network.Wai.Application.Classic (Logger)
import System.Posix (ProcessID)

data FileLogSpec = FileLogSpec {
    log_file :: String
  , log_file_size :: Integer
  , log_backup_number :: Int
  }

data LogType = LogNone | LogStdout | LogFile FileLogSpec

type LogController = [ProcessID] -> IO ()

data LogStr = LS !String | LB !ByteString
