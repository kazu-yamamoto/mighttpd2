{-# LANGUAGE OverloadedStrings #-}

module Report where

import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.UnixTime
import System.Posix
import Utils

reportFile :: FilePath
reportFile = "/tmp/mighty_report"

report :: ByteString -> IO ()
report msg = handle ignore $ do
    pid <- BS.pack . show <$> getProcessID
    tm <- formatUnixTime "%d %b %Y %H:%M:%S" <$> getUnixTime
    BS.appendFile reportFile $ BS.concat [tm, ": pid = ", pid, ": ", msg, "\n"]


