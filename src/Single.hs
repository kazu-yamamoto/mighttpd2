{-# LANGUAGE OverloadedStrings, CPP#-}

module Single (single, mainLoop, closeService, ifRouteFileIsValid) where

import Config
import Route
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Conduit.Network
import Network
import qualified Network.HTTP.Conduit as H
import Network.HTTP.Date
#if DEBUG
import Network.TLS
#endif
import Network.Wai.Application.Classic hiding ((</>), (+++))
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import System.Date.Cache
import System.Exit
import System.Posix
import System.IO.Error (ioeGetErrorString)

import FileCGIApp
import FileCache
import Log
import Report
import Resource (setGroupUser)
import Signal
import State
import Types
import Utils

----------------------------------------------------------------

single :: Option -> RouteDB -> Service -> Reporter -> Stater -> Logger -> IO ()
single opt route service rpt stt lgr = reportDo rpt $ do
    setGroupUser opt -- don't change the user of the master process
    ignoreSigChild
    getInfo <- fileCacheInit
    mgr <- H.newManager H.def {
            -- FIXME
            H.managerConnCount = 1024
          }
    setHandler sigStop   stopHandler
    setHandler sigRetire retireHandler
    setHandler sigReload (reloadHandler lgr getInfo mgr)
    setHandler sigInfo   infoHandler
    report rpt "Worker Mighty started"
    reload opt route service rpt stt lgr getInfo mgr
  where
    stopHandler = Catch $ do
        report rpt "Worker Mighty finished"
        finReporter rpt
        finLogger lgr
        closeService service
        exitImmediately ExitSuccess
    retireHandler = Catch $ ifWarpThreadsAreActive stt $ do
        report rpt "Worker Mighty retiring"
        closeService service
        goRetiring stt
    reloadHandler lggr getInfo mgr = Catch $ ifWarpThreadsAreActive stt $
        ifRouteFileIsValid rpt opt $ \newroute -> do
            report rpt "Worker Mighty reloaded"
            void . forkIO $ reload opt newroute service rpt stt lggr getInfo mgr
    infoHandler = Catch $ do
        i <- bshow <$> getConnectionCounter stt
        status <- bshow <$> getServerStatus stt
        report rpt $ status +++ ": # of connections = " +++ i

ifRouteFileIsValid :: Reporter -> Option -> (RouteDB -> IO ()) -> IO ()
ifRouteFileIsValid rpt opt act = case opt_routing_file opt of
    Nothing    -> return ()
    Just rfile -> try (parseRoute rfile) >>= either reportError act
  where
    reportError = report rpt . BS.pack . ioeGetErrorString

----------------------------------------------------------------

reload :: Option -> RouteDB -> Service
       -> Reporter -> Stater -> Logger
       -> (Path -> IO FileInfo) -> H.Manager
       -> IO ()
reload opt route service rpt stt lgr getInfo mgr = reportDo rpt $ do
    setMyWarpThreadId stt
    zdater <- initZoneDater
    let app req = fileCgiApp (cspec zdater) filespec cgispec revproxyspec route req
    case service of
        HttpOnly s  -> runSettingsSocket setting s app
        HttpsOnly s -> runTLSSocket tlsSetting setting s app
        HttpAndHttps s1 s2 -> do
            tid <- forkIO $ runSettingsSocket setting s1 app
            addAnotherWarpThreadId stt tid
            runTLSSocket tlsSetting setting s2 app
  where
    debug = opt_debug_mode opt
    setting = defaultSettings {
        settingsPort        = opt_port opt
      , settingsOnException = if debug then printStdout else warpHandler rpt
      , settingsOnOpen      = increment stt
      , settingsOnClose     = decrement stt
      , settingsTimeout     = opt_connection_timeout opt
      , settingsHost        = HostAny
      , settingsFdCacheDuration     = opt_fd_cache_duration opt
      , settingsResourceTPerRequest = False
      }
    tlsSetting = defaultTlsSettings {
        certFile = opt_tls_cert_file opt
      , keyFile  = opt_tls_key_file opt
#if DEBUG
      , tlsLogging = tlsLogger
#endif
      }
    serverName = BS.pack $ opt_server_name opt
    cspec zdater = ClassicAppSpec {
        softwareName = serverName
      , logger = apatcheLogger lgr
      , dater = zdater
      , statusFileDir = fromString $ opt_status_file_dir opt
      }
    filespec = FileAppSpec {
        indexFile = fromString $ opt_index_file opt
      , isHTML = \x -> ".html" `isSuffixOf` x || ".htm" `isSuffixOf` x
      , getFileInfo = getInfo
      }
    cgispec = CgiAppSpec {
        indexCgi = "index.cgi"
      }
    revproxyspec = RevProxyAppSpec {
        revProxyManager = mgr
      }
    initZoneDater = fst <$> clockDateCacher DateCacheConf {
        getTime = epochTime
      , formatDate = return . formatHTTPDate . epochTimeToHTTPDate
      }

----------------------------------------------------------------

mainLoop :: Reporter -> Stater -> Logger -> IO ()
mainLoop rpt stt lgr = do
    threadDelay 1000000
    retiring <- isRetiring stt
    counter <- getConnectionCounter stt
    if retiring && counter == 0 then do
        report rpt "Worker Mighty retired"
        finReporter rpt
        finLogger lgr
        exitSuccess
      else
        mainLoop rpt stt lgr

----------------------------------------------------------------

closeService :: Service -> IO ()
closeService (HttpOnly s) = sClose s
closeService (HttpsOnly s) = sClose s
closeService (HttpAndHttps s1 s2) = sClose s1 >> sClose s2

#if DEBUG
tlsLogger :: Logging
tlsLogger = Logging	{
    loggingPacketSent = putStrLn
  , loggingPacketRecv = putStrLn
  , loggingIOSent = BS.putStrLn
  , loggingIORecv = \_ s -> BS.putStrLn s
  }
#endif
