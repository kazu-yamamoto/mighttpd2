{-# LANGUAGE OverloadedStrings #-}

module Report where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.UnixTime
import System.Posix

reportFile :: FilePath
reportFile = "/tmp/mighty_report"

report :: ByteString -> IO ()
report msg = do
    pid <- BS.pack . show <$> getProcessID
    tm <- formatUnixTime mailDateFormat <$> getUnixTime
    BS.appendFile reportFile $ BS.concat [tm, ": pid = ", pid, ": ", msg, "\n"]


