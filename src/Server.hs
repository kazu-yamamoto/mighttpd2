{-# LANGUAGE OverloadedStrings, CPP, BangPatterns #-}

module Server (server, defaultDomain, defaultPort) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (try)
import Control.Monad (void, unless, when)
import qualified Data.ByteString.Char8 as BS (pack)
import Network (Socket, sClose)
import qualified Network.HTTP.Client.Manager as H
import Network.Wai.Application.Classic hiding ((</>), (+++))
import Network.Wai.Handler.Warp
import System.Exit (ExitCode(..), exitSuccess)
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Log.FastLogger.File
import System.Posix (exitImmediately, Handler(..), getProcessID, setFileMode)
import System.Posix.Signals (sigCHLD)
import Network.Wai.Logger

#ifdef TLS
import Network.Wai.Handler.WarpTLS
#endif

import Program.Mighty

import WaiApp

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

longTimerInterval :: Int
longTimerInterval = 10

logBufferSize :: Int
logBufferSize = 4 * 1024 * 10

managerNumber :: Int
managerNumber = 1024 -- FIXME

----------------------------------------------------------------

server :: Option -> Reporter -> RouteDB -> IO ()
server opt rpt route = reportDo rpt $ do
    unlimit openFileNumber
    svc <- openService opt
    unless debug writePidFile
    setGroupUser (opt_user opt) (opt_group opt)
    logCheck logtype
    stt <- initStater
    (zdater,zupdater) <- clockDateCacher
    (lgr,flusher,rotator) <- initLogger FromSocket logtype zdater
    (getInfo,cleaner) <- fileCacheInit
    mgr <- getManager opt
    let mighty = reload opt rpt svc stt lgr getInfo mgr
    setHandlers opt rpt svc stt flusher mighty
    report rpt "Mighty started"
    void . forkIO $ mighty route
    mainLoop rpt stt cleaner flusher debug rotator zupdater 0
  where
    debug = opt_debug_mode opt
    port = opt_port opt
    pidfile
        | port == 80 = opt_pid_file opt
        | otherwise  = opt_pid_file opt ++ show port
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

setHandlers :: Option -> Reporter -> Service -> Stater -> LogFlusher -> Mighty -> IO ()
setHandlers opt rpt svc stt flusher mighty = do
    setHandler sigStop   stopHandler
    setHandler sigRetire retireHandler
    setHandler sigInfo   infoHandler
    setHandler sigReload reloadHandler
    setHandler sigCHLD   Ignore        -- for CGI
  where
    stopHandler = Catch $ do
        report rpt "Mighty finished"
        finReporter rpt
        closeService svc
        flusher -- FIXME and close?
        exitImmediately ExitSuccess
    retireHandler = Catch $ ifWarpThreadsAreActive stt $ do
        report rpt "Mighty retiring"
        closeService svc
        flusher -- FIXME and close?
        goRetiring stt
    infoHandler = Catch $ do
        i <- bshow <$> getConnectionCounter stt
        status <- bshow <$> getServerStatus stt
        report rpt $ status +++ ": # of connections = " +++ i
    reloadHandler = Catch $ ifWarpThreadsAreActive stt $
        ifRouteFileIsValid rpt opt $ \newroute -> do
            report rpt "Mighty reloaded"
            void . forkIO $ mighty newroute

----------------------------------------------------------------

type Mighty = RouteDB -> IO ()

ifRouteFileIsValid :: Reporter -> Option -> Mighty -> IO ()
ifRouteFileIsValid rpt opt act = case opt_routing_file opt of
    Nothing    -> return ()
    Just rfile -> try (parseRoute rfile defaultDomain defaultPort) >>= either reportError act
  where
    reportError = report rpt . BS.pack . ioeGetErrorString

----------------------------------------------------------------

reload :: Option -> Reporter -> Service -> Stater
       -> ApacheLogger -> GetInfo -> ConnPool
       -> Mighty
reload opt rpt svc stt lgr getInfo _mgr route = reportDo rpt $ do
    setMyWarpThreadId stt
    let app req = fileCgiApp cspec filespec cgispec revproxyspec route req
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
      }
    serverName = BS.pack $ opt_server_name opt
    cspec = ClassicAppSpec {
        softwareName = serverName
      , logger = lgr
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
        revProxyManager = _mgr
      }
#ifdef TLS
    tlsSetting = defaultTlsSettings {
        certFile = opt_tls_cert_file opt
      , keyFile  = opt_tls_key_file opt
      }
#endif

----------------------------------------------------------------

mainLoop :: Reporter -> Stater -> RemoveInfo
         -> LogFlusher -> Bool ->  LogRotator
         -> DateCacheUpdater
         -> Int -> IO ()
mainLoop rpt stt cleaner flusher everySecond rotator zupdater sec = do
    threadDelay oneSecond
    retiring <- isRetiring stt
    counter <- getConnectionCounter stt
    if retiring && counter == 0 then do
        report rpt "Mighty retired"
        finReporter rpt
        flusher
        exitSuccess
      else do
        zupdater
        when everySecond $ flusher
        let longTimer = sec == longTimerInterval
        when longTimer $ do
            unless everySecond $ flusher
            cleaner
            rotator
        let !next = if longTimer then 0 else sec + 1
        mainLoop rpt stt cleaner flusher everySecond rotator zupdater next

----------------------------------------------------------------

data Service = HttpOnly Socket | HttpsOnly Socket | HttpAndHttps Socket Socket

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

type ConnPool = H.Manager

getManager :: Option -> IO ConnPool
getManager opt = H.newManager H.defaultManagerSettings {
    H.managerConnCount = managerNumber
  , H.managerResponseTimeout = if opt_proxy_timeout opt == 0 then
                                   H.managerResponseTimeout H.defaultManagerSettings
                                 else
                                   Just (opt_proxy_timeout opt)
  }
