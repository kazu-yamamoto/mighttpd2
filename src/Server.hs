{-# LANGUAGE OverloadedStrings, CPP #-}

module Server (server, defaultDomain, defaultPort) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (try)
import Control.Monad (void, unless, when)
import qualified Data.ByteString.Char8 as BS (pack)
import Network (Socket, sClose)
import Network.HTTP.Date (formatHTTPDate, epochTimeToHTTPDate)
import Network.Wai.Application.Classic hiding ((</>), (+++))
import Network.Wai.Handler.Warp
import System.Exit (ExitCode(..), exitSuccess)
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Posix (exitImmediately, Handler(..), epochTime)
import System.Posix (getProcessID, setFileMode)
#ifdef REV_PROXY
import qualified Network.HTTP.Conduit as H
#endif
#ifdef TLS
import Network.Wai.Handler.WarpTLS
#endif

import Program.Mighty

import WaiApp

----------------------------------------------------------------

data Service = HttpOnly Socket | HttpsOnly Socket | HttpAndHttps Socket Socket

----------------------------------------------------------------

defaultDomain :: Domain
defaultDomain = "localhost"

defaultPort :: Int
defaultPort = 80

backlogNumber :: Int
backlogNumber = 2048

openFileNumber :: Integer
openFileNumber = 10000

oneSecond :: Int
oneSecond = 1000000

logBufferSize :: Int
logBufferSize = 4 * 1024 * 10

----------------------------------------------------------------

#ifdef REV_PROXY
type ConnPool = H.Manager
#else
type ConnPool = ()
#endif

----------------------------------------------------------------

server :: Option -> RouteDB -> Reporter -> IO ()
server opt route rpt = reportDo rpt $ do
    unlimit openFileNumber
    svc <- openService opt
    unless debug writePidFile
    logCheck logtype
    stt <- initStater
    (lgr,flusher,updater) <- initLogger FromSocket logtype -- FIXME
    -- killed by signal
    setGroupUser (opt_user opt) (opt_group opt) -- don't change the user of the master process
    (getInfo, _fixme) <- fileCacheInit
#ifdef REV_PROXY
    mgr <- H.newManager H.def { H.managerConnCount = 1024 } -- FIXME
#else
    let mgr = ()
#endif
    setHandlers opt rpt svc stt lgr getInfo mgr
    report rpt "Worker Mighty started"
    void . forkIO $ reload opt route svc rpt stt lgr getInfo mgr -- FIXME
    mainLoop rpt stt flusher
  where
    debug = opt_debug_mode opt
    pidfile = opt_pid_file opt
    writePidFile = do
        pid <- getProcessID
        writeFile pidfile $ show pid ++ "\n"
        setFileMode pidfile 0o644
    logspec = FileLogSpec {
        log_file          = opt_log_file opt
      , log_file_size     = fromIntegral $ opt_log_file_size opt
      , log_backup_number = opt_log_backup_number opt
      }
    logtype
      | not (opt_logging opt) = LogNone
      | debug                 = LogStdout logBufferSize
      | otherwise             = LogFile logspec logBufferSize

setHandlers :: Option -> Reporter -> Service -> Stater -> ApacheLogger -> (Path -> IO FileInfo) -> ConnPool -> IO ()
setHandlers opt rpt svc stt lgr getInfo mgr = do
    setHandler sigStop   stopHandler
    setHandler sigRetire retireHandler
    setHandler sigInfo   infoHandler
    setHandler sigReload reloadHandler
  where
    stopHandler = Catch $ do
        report rpt "Worker Mighty finished"
        finReporter rpt
--        finLogger lgr -- FIXME
        closeService svc
        exitImmediately ExitSuccess
    retireHandler = Catch $ ifWarpThreadsAreActive stt $ do
        report rpt "Worker Mighty retiring"
        closeService svc
        goRetiring stt
    infoHandler = Catch $ do
        i <- bshow <$> getConnectionCounter stt
        status <- bshow <$> getServerStatus stt
        report rpt $ status +++ ": # of connections = " +++ i
    reloadHandler = Catch $ ifWarpThreadsAreActive stt $
        ifRouteFileIsValid rpt opt $ \newroute -> do
            report rpt "Worker Mighty reloaded"
            void . forkIO $ reload opt newroute svc rpt stt lgr getInfo mgr

----------------------------------------------------------------

ifRouteFileIsValid :: Reporter -> Option -> (RouteDB -> IO ()) -> IO ()
ifRouteFileIsValid rpt opt act = case opt_routing_file opt of
    Nothing    -> return ()
    Just rfile -> try (parseRoute rfile defaultDomain defaultPort) >>= either reportError act
  where
    reportError = report rpt . BS.pack . ioeGetErrorString

----------------------------------------------------------------

reload :: Option -> RouteDB -> Service
       -> Reporter -> Stater -> ApacheLogger
       -> (Path -> IO FileInfo) -> ConnPool
       -> IO ()
reload opt route svc rpt stt lgr getInfo _mgr = reportDo rpt $ do
    setMyWarpThreadId stt
    zdater <- initZoneDater -- FIXME
#ifdef REV_PROXY
    let app req = fileCgiApp (cspec zdater) filespec cgispec revproxyspec route req
#else
    let app req = fileCgiApp (cspec zdater) filespec cgispec route req
#endif
    case svc of
        HttpOnly s  -> runSettingsSocket setting s app
#ifdef TLS
        HttpsOnly s -> runTLSSocket tlsSetting setting s app
        HttpAndHttps s1 s2 -> do
            tid <- forkIO $ runSettingsSocket setting s1 app
            addAnotherWarpThreadId stt tid
            runTLSSocket tlsSetting setting s2 app
#else
        _ -> error "never reach"
#endif
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
    serverName = BS.pack $ opt_server_name opt
    cspec zdater = ClassicAppSpec {
        softwareName = serverName
      , logger = lgr
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
    initZoneDater = fst <$> clockDateCacher DateCacheConf {
        getTime = epochTime
      , formatDate = return . formatHTTPDate . epochTimeToHTTPDate
      }
#ifdef REV_PROXY
    revproxyspec = RevProxyAppSpec {
        revProxyManager = _mgr
      }
#endif
#ifdef TLS
    tlsSetting = defaultTlsSettings {
        certFile = opt_tls_cert_file opt
      , keyFile  = opt_tls_key_file opt
      }
#endif

----------------------------------------------------------------

openService :: Option -> IO Service
openService opt
  | service == 1 = do
      s <- listenSocket httpsPort backlogNumber
      debugMessage $ "HTTP/TLS service on port " ++ httpsPort ++ "."
      return $ HttpsOnly s
  | service == 2 = do
      s1 <- listenSocket httpPort backlogNumber
      s2 <- listenSocket httpsPort backlogNumber
      debugMessage $ "HTTP service on port " ++ httpPort ++ " and "
                  ++ "HTTP/TLS service on port " ++ httpsPort ++ "."
      return $ HttpAndHttps s1 s2
  | otherwise = do
      s <- listenSocket httpPort backlogNumber
      debugMessage $ "HTTP service on port " ++ httpPort ++ "."
      return $ HttpOnly s
  where
    httpPort  = show $ opt_port opt
    httpsPort = show $ opt_tls_port opt
    service = opt_service opt
    debug = opt_debug_mode opt
    debugMessage msg = when debug $ do
        putStrLn msg
        hFlush stdout

----------------------------------------------------------------

closeService :: Service -> IO ()
closeService (HttpOnly s) = sClose s
closeService (HttpsOnly s) = sClose s
closeService (HttpAndHttps s1 s2) = sClose s1 >> sClose s2


----------------------------------------------------------------

-- FIXME log controller should be implemented here
mainLoop :: Reporter -> Stater -> LogFlusher -> IO ()
mainLoop rpt stt flusher = do
    threadDelay oneSecond
    retiring <- isRetiring stt
    counter <- getConnectionCounter stt
    if retiring && counter == 0 then do
        report rpt "Worker Mighty retired"
        finReporter rpt
        flusher
        exitSuccess
      else
        mainLoop rpt stt flusher
