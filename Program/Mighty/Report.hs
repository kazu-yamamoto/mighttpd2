{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Program.Mighty.Report (
    Reporter
  , initReporter
  , report
  , reportDo
  , warpHandler
  , printStdout
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Exception
import qualified Control.Exception as E (catch)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.UnixTime
import GHC.IO.Exception (IOErrorType(..))
import Network.Wai
import Network.Wai.Handler.Warp (InvalidRequest)
import Network.Wai.Handler.Warp.Internal (TimeoutThread(..))
import System.IO
import System.IO.Error (ioeGetErrorType)
import System.Posix (getProcessID)

import Program.Mighty.ByteString

data Method = FileOnly | FileAndStdout deriving Eq
data Reporter = Reporter Method FilePath

initReporter :: Bool -> FilePath -> Reporter
initReporter debug reportFile = Reporter method reportFile
  where
    method
      | debug     = FileAndStdout
      | otherwise = FileOnly

report :: Reporter -> ByteString -> IO ()
report (Reporter method reportFile) msg = handle (\(SomeException _) -> return ()) $ do
    pid <- BS.pack . show <$> getProcessID
    tm <- getUnixTime >>= formatUnixTime "%d %b %Y %H:%M:%S"
    let logmsg = BS.concat [tm, ": pid = ", pid, ": ", msg, "\n"]
    BS.appendFile reportFile logmsg
    when (method == FileAndStdout) $ BS.putStr logmsg

----------------------------------------------------------------

reportDo :: Reporter -> IO () -> IO ()
reportDo rpt act = act `E.catch` warpHandler rpt Nothing

----------------------------------------------------------------

warpHandler :: Reporter -> Maybe Request -> SomeException -> IO ()
warpHandler rpt _ e = throwIO e `catches` handlers
  where
    handlers = [Handler ah, Handler th, Handler ih, Handler oh, Handler sh]
    ah :: AsyncException -> IO ()
    ah ThreadKilled = norecode
    ah x            = recode x
    th :: TimeoutThread -> IO ()
    th TimeoutThread = norecode
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

----------------------------------------------------------------

printStdout :: Maybe Request -> SomeException -> IO ()
printStdout _ x = print x >> hFlush stdout
