{-# LANGUAGE OverloadedStrings #-}

module Report where

import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.UnixTime
import System.IO
import System.Posix
import Utils

reportFile :: FilePath
reportFile = "/tmp/mighty_report"

report :: Handle -> ByteString -> IO ()
report rpthdl msg = handle ignore $ do
    pid <- BS.pack . show <$> getProcessID
    tm <- formatUnixTime "%d %b %Y %H:%M:%S" <$> getUnixTime
    let logmsg = BS.concat [tm, ": pid = ", pid, ": ", msg, "\n"]
    BS.hPutStrLn rpthdl logmsg
    hFlush rpthdl
