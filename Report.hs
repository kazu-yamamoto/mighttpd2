{-# LANGUAGE OverloadedStrings #-}

module Report (
    Reporter
  , initReporter
  , finReporter
  , report
  , reportFile
  ) where

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

newtype Reporter = Reporter Handle

initReporter :: IO (Either SomeException Reporter)
initReporter = try $ Reporter <$> openFile reportFile AppendMode

finReporter :: Reporter -> IO ()
finReporter (Reporter rpthdl) = hClose rpthdl

report :: Reporter -> ByteString -> IO ()
report (Reporter rpthdl) msg = handle ignore $ do
    pid <- BS.pack . show <$> getProcessID
    tm <- formatUnixTime "%d %b %Y %H:%M:%S" <$> getUnixTime
    let logmsg = BS.concat [tm, ": pid = ", pid, ": ", msg, "\n"]
    BS.hPutStrLn rpthdl logmsg
    hFlush rpthdl
