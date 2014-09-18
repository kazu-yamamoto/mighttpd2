{-# LANGUAGE OverloadedStrings, CPP, BangPatterns #-}

module Server (server, defaultDomain, defaultPort) where

import Control.Concurrent (runInUnboundThread)
import Control.Exception (try)
import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Streaming.Network (bindPortTCP)
import Network (Socket, sClose)
import qualified Network.HTTP.Client as H
import Network.Wai.Application.Classic hiding ((</>), (+++))
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import System.Exit (ExitCode(..), exitSuccess)
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Posix (exitImmediately, Handler(..), getProcessID, setFileMode)
import System.Posix.Signals (sigCHLD)

#ifdef TLS
import Control.Concurrent.Async (concurrently)
import Network.Wai.Handler.WarpTLS
#endif

import Program.Mighty

import WaiApp

----------------------------------------------------------------

defaultDomain :: Domain
defaultDomain = "localhost"

defaultPort :: Int
defaultPort = 80

openFileNumber :: Integer
openFileNumber = 10000

logBufferSize :: Int
logBufferSize = 4 * 1024 * 10

managerNumber :: Int
managerNumber = 1024 -- FIXME

----------------------------------------------------------------

type LogRemover = IO ()

----------------------------------------------------------------

server :: Option -> Reporter -> RouteDB -> IO ()
server opt rpt route = reportDo rpt $ do
    unlimit openFileNumber
    svc <- openService opt
    unless debug writePidFile
    rdr <- newRouteDBRef route
    setGroupUser (opt_user opt) (opt_group opt)
    logCheck logtype
    (zdater,_) <- clockDateCacher
    ap <- initLogger FromSocket logtype zdater
    let lgr = apacheLogger ap
        remover = logRemover ap
    getInfo <- fileCacheInit
    mgr <- getManager opt
    setHandlers opt rpt svc remover rdr

    report rpt "Mighty started"
    runInUnboundThread $ mighty opt rpt svc lgr getInfo mgr rdr
    report rpt "Mighty retired"
    finReporter rpt
    remover
    exitSuccess
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

setHandlers :: Option -> Reporter -> Service -> LogRemover -> RouteDBRef -> IO ()
setHandlers opt rpt svc remover rdr = do
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
        remover
        exitImmediately ExitSuccess
    retireHandler = Catch $ do
        report rpt "Mighty retiring"
        closeService svc -- this lets warp break
    infoHandler = Catch $ report rpt "obsolted"
    reloadHandler = Catch $ do
        ifRouteFileIsValid rpt opt $ \newroute -> do
            writeRouteDBRef rdr newroute
            report rpt "Mighty reloaded"

----------------------------------------------------------------

ifRouteFileIsValid :: Reporter -> Option -> (RouteDB -> IO ()) -> IO ()
ifRouteFileIsValid rpt opt act = case opt_routing_file opt of
    Nothing    -> return ()
    Just rfile -> try (parseRoute rfile defaultDomain defaultPort) >>= either reportError act
  where
    reportError = report rpt . BS.pack . ioeGetErrorString

----------------------------------------------------------------

mighty :: Option -> Reporter -> Service
       -> ApacheLogger -> GetInfo -> ConnPool -> RouteDBRef
       -> IO ()
mighty opt rpt svc lgr getInfo _mgr rdr = reportDo rpt $ case svc of
    HttpOnly s  -> runSettingsSocket setting s app
#ifdef TLS
    HttpsOnly s -> runTLSSocket tlsSetting setting s app
    HttpAndHttps s1 s2 -> concurrently
        (runSettingsSocket setting s1 app)
        (runTLSSocket tlsSetting setting s2 app)
#else
    _ -> error "never reach"
#endif
  where
    app req = fileCgiApp cspec filespec cgispec revproxyspec rdr req
    debug = opt_debug_mode opt
    -- We don't use setInstallShutdownHandler because we may use
    -- two sockets for HTTP and HTTPS.
    setting = setPort            (opt_port opt) -- just in case
            $ setHost            (fromString (opt_host opt))  -- just in case
            $ setOnException     (if debug then printStdout else warpHandler rpt)
            $ setTimeout         (opt_connection_timeout opt) -- seconds
            $ setFdCacheDuration (opt_fd_cache_duration opt)
            defaultSettings
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

data Service = HttpOnly Socket | HttpsOnly Socket | HttpAndHttps Socket Socket

----------------------------------------------------------------

openService :: Option -> IO Service
openService opt
  | service == 1 = do
      s <- bindPortTCP httpsPort hostpref
      debugMessage $ "HTTP/TLS service on port " ++ show httpsPort ++ "."
      return $ HttpsOnly s
  | service == 2 = do
      s1 <- bindPortTCP httpPort hostpref
      s2 <- bindPortTCP httpsPort hostpref
      debugMessage $ "HTTP service on port " ++ show httpPort ++ " and "
                  ++ "HTTP/TLS service on port " ++ show httpsPort ++ "."
      return $ HttpAndHttps s1 s2
  | otherwise = do
      s <- bindPortTCP httpPort hostpref
      debugMessage $ "HTTP service on port " ++ show httpPort ++ "."
      return $ HttpOnly s
  where
    httpPort  = opt_port opt
    httpsPort = opt_tls_port opt
    hostpref  = fromString $ opt_host opt
    service = opt_service opt
    debug = opt_debug_mode opt
    debugMessage msg = when debug $ do
        putStrLn msg
        hFlush stdout

----------------------------------------------------------------

closeService :: Service -> IO ()
closeService (HttpOnly s)         = sClose s
closeService (HttpsOnly s)        = sClose s
closeService (HttpAndHttps s1 s2) = sClose s1 >> sClose s2

----------------------------------------------------------------

type ConnPool = H.Manager

getManager :: Option -> IO ConnPool
getManager opt = H.newManager H.defaultManagerSettings {
    H.managerConnCount = managerNumber
  , H.managerResponseTimeout = responseTimeout
  }
  where
    responseTimeout
      | opt_proxy_timeout opt == 0 = H.managerResponseTimeout H.defaultManagerSettings
      | otherwise                  = Just (opt_proxy_timeout opt * 1000000) -- micro seconds
