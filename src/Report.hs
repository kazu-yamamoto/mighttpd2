{-# LANGUAGE OverloadedStrings #-}

module Report (
    Reporter
  , initReporter
  , finReporter
  , report
  , reportFile
  , reportDo
  , warpHandler
  , ifRouteFileIsValid
  ) where

import Control.Applicative
import Control.Exception
import qualified Control.Exception as E (catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.UnixTime
import GHC.IO.Exception (IOErrorType(..))
import Network.Wai.Handler.Warp (InvalidRequest)
import System.IO
import System.IO.Error (ioeGetErrorType, ioeGetErrorString)
import System.Posix (getProcessID)

import Config
import Route
import Types (RouteDB)
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
    BS.hPutStr rpthdl logmsg
    hFlush rpthdl

----------------------------------------------------------------

reportDo :: Reporter -> IO () -> IO ()
reportDo rpt act = act `E.catch` warpHandler rpt

----------------------------------------------------------------

warpHandler :: Reporter -> SomeException -> IO ()
warpHandler rpt e = throwIO e `catches` handlers
  where
    handlers = [Handler ah, Handler ih, Handler oh, Handler sh]
    ah :: AsyncException -> IO ()
    ah ThreadKilled = norecode
    ah x            = recode x
    ih :: InvalidRequest -> IO ()
    ih _ = norecode
    oh :: IOException -> IO ()
    oh x
      | et `elem` ignEts = norecode
      | otherwise        = recode x
      where
        et = ioeGetErrorType x
        ignEts = [ResourceVanished, InvalidArgument]
    sh :: SomeException -> IO ()
    sh x = recode x
    norecode = return ()
    recode :: Exception e => e -> IO ()
    recode   = report rpt . bshow

ifRouteFileIsValid :: Reporter -> Option -> (RouteDB -> IO ()) -> IO ()
ifRouteFileIsValid rpt opt act =
    return (opt_routing_file opt) >>>= \rfile ->
    try (parseRoute rfile) >>= either reportError act
  where
    reportError = report rpt . BS.pack . ioeGetErrorString
