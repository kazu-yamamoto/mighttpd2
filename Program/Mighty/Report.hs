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
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.UnixTime
import GHC.IO.Exception (IOErrorType (..))
import Network.Wai
import Network.Wai.Handler.Warp (InvalidRequest)
import System.Exit (ExitCode)
import System.IO
import System.IO.Error (ioeGetErrorType)
import System.Posix (getProcessID)
import System.Posix.IO (
    OpenFileFlags (..),
    OpenMode (WriteOnly),
    defaultFileFlags,
    fdToHandle,
    openFd,
 )
#if !MIN_VERSION_unix(2,8,0)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)
#endif

import Network.HTTP2.Client (HTTP2Error)
#ifdef HTTP_OVER_TLS
import Network.TLS (TLSException)
import Network.Wai.Handler.WarpTLS (WarpTLSException)
#ifdef HTTP_OVER_QUIC
import Network.QUIC (QUICException)
#endif
#endif

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
report (Reporter method reportFile) msg = E.handle (\(E.SomeException _) -> return ()) $ do
    pid <- BS.pack . show <$> getProcessID
    tm <- getUnixTime >>= formatUnixTime "%d %b %Y %H:%M:%S"
    let logmsg = BS.concat [tm, ": pid = ", pid, ": ", msg, "\n"]
    -- Flushing since stdout may be a file and Mighty may exit immediately.
    when (method == FileAndStdout) $ BS.putStr logmsg >> hFlush stdout
    appendNoFollow reportFile logmsg

-- | Appending without following a symbolic link since the report
--   file may be in a world-writable directory such as /tmp.
appendNoFollow :: FilePath -> ByteString -> IO ()
appendNoFollow file msg = E.bracket open hClose (`BS.hPut` msg)
  where
#if MIN_VERSION_unix(2,8,0)
    open = openFd file WriteOnly flags >>= fdToHandle
    flags = defaultFileFlags{append = True, nofollow = True, creat = Just 0o644}
#else
    open = do
        isLink <- (isSymbolicLink <$> getSymbolicLinkStatus file)
            `E.catch` \(_ :: E.IOException) -> return False
        when isLink $ E.throwIO $ userError "symbolic link"
        openFd file WriteOnly (Just 0o644) flags >>= fdToHandle
    flags = defaultFileFlags{append = True}
#endif

----------------------------------------------------------------

reportDo :: Reporter -> IO () -> IO ()
reportDo rpt act = act `E.catch` warpHandler rpt Nothing

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
warpHandler :: Reporter -> Maybe Request -> E.SomeException -> IO ()
warpHandler rpt _ se
    | Just (_ :: ExitCode)         <- E.fromException se = return ()
    | Just (e :: E.IOException)    <- E.fromException se =
        if ioeGetErrorType e `elem` [ResourceVanished, InvalidArgument]
            then return ()
            else report rpt $ bshow se
    | Just (_ :: InvalidRequest)   <- E.fromException se = return () -- Warp
    | Just (_ :: HTTP2Error)       <- E.fromException se = return ()
#ifdef HTTP_OVER_TLS
    | Just (_ :: TLSException)     <- E.fromException se = return ()
    | Just (_ :: WarpTLSException) <- E.fromException se = return ()
#ifdef HTTP_OVER_QUIC
    | Just (_ :: QUICException)    <- E.fromException se = return ()
#endif
#endif
    | otherwise = report rpt $ bshow se
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

printStdout :: Maybe Request -> E.SomeException -> IO ()
printStdout _ x = print x >> hFlush stdout
