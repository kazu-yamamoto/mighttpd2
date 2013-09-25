{-# LANGUAGE OverloadedStrings #-}

module Report (
    Reporter
  , initReporter
  , finReporter
  , report
  , reportDo
  , warpHandler
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
import System.IO.Error (ioeGetErrorType)
import System.Posix (getProcessID)

import Program.Mighty

import Utils

newtype Reporter = Reporter Handle

initReporter :: FilePath -> IO (Either SomeException Reporter)
initReporter reportFile = try $ Reporter <$> openFile reportFile AppendMode

finReporter :: Reporter -> IO ()
finReporter (Reporter rpthdl) = hClose rpthdl

report :: Reporter -> ByteString -> IO ()
report (Reporter rpthdl) msg = handle ignore $ do
    pid <- BS.pack . show <$> getProcessID
    tm <- getUnixTime >>= formatUnixTime "%d %b %Y %H:%M:%S"
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
