{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Program.Mighty.Report (
    Reporter,
    initReporter,
    report,
    reportDo,
    warpHandler,
    printStdout,
) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.UnixTime
import GHC.IO.Exception (IOErrorType (..))
import Network.Wai
import Network.Wai.Handler.Warp (InvalidRequest)
import System.IO
import System.IO.Error (ioeGetErrorType)
import System.Posix (getProcessID)
import UnliftIO.Exception

import Program.Mighty.ByteString

data Method = FileOnly | FileAndStdout deriving (Eq)
data Reporter = Reporter Method FilePath

initReporter :: Bool -> FilePath -> Reporter
initReporter debug reportFile = Reporter method reportFile
  where
    method
        | debug = FileAndStdout
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
reportDo rpt act = act `catchAny` warpHandler rpt Nothing

----------------------------------------------------------------

warpHandler :: Reporter -> Maybe Request -> SomeException -> IO ()
warpHandler rpt _ se
    | Just (_ :: InvalidRequest) <- fromException se = return ()
    | Just (e :: IOException) <- fromException se =
        if ioeGetErrorType e `elem` [ResourceVanished, InvalidArgument]
            then return ()
            else report rpt $ bshow se
    | otherwise = report rpt $ bshow se

----------------------------------------------------------------

printStdout :: Maybe Request -> SomeException -> IO ()
printStdout _ x = print x >> hFlush stdout
