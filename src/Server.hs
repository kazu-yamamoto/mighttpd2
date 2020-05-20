{-# LANGUAGE OverloadedStrings, CPP, BangPatterns #-}

module Server (server, defaultDomain, defaultPort) where

import Control.Concurrent (runInUnboundThread)
import Control.Exception (try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
#ifdef HTTP_OVER_TLS
import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd, break)
#endif
import Data.Streaming.Network (bindPortTCP)
import GHC.Natural (Natural, naturalToInt)
import Network.Socket (Socket, close)
import qualified Network.HTTP.Client as H
import Network.Wai.Application.Classic hiding ((</>))
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import System.Exit (ExitCode(..), exitSuccess)
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Posix (exitImmediately, Handler(..), getProcessID, setFileMode)
import System.Posix.Signals (sigCHLD)

import Program.Mighty
import WaiApp

import qualified Network.Wai.Middleware.Push.Referer as P

#ifdef HTTP_OVER_TLS
import Control.Concurrent.Async (concurrently_)
import Control.Monad (void)
import Network.Wai.Handler.WarpTLS
import Network.TLS.SessionManager
#ifdef HTTP_OVER_QUIC
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import qualified Control.Exception as E
import Data.ByteString.Base16 (encode)
import qualified Network.QUIC as Q
import qualified Network.TLS.SessionManager as SM
import Network.Wai.Handler.WarpQUIC
import System.FilePath
#endif
#else
data TLSSettings = TLSSettings
#endif

----------------------------------------------------------------

defaultDomain :: Domain
defaultDomain = "localhost"

defaultPort :: Natural
defaultPort = 80

openFileNumber :: Integer
openFileNumber = 10000

logBufferSize :: Natural
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
    tlsSetting <- getTlsSetting opt
    setGroupUser (opt_user opt) (opt_group opt)
    logCheck logtype
    (zdater,_) <- clockDateCacher
    ap <- initLogger FromSocket logtype zdater
    let lgr = apacheLogger ap
        remover = logRemover ap
        pushlgr = serverpushLogger ap
    mgr <- getManager opt
    setHandlers opt rpt svc remover rdr

    report rpt "Mighty started"
    runInUnboundThread $ mighty opt rpt svc lgr pushlgr mgr rdr tlsSetting
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
      , log_backup_number = naturalToInt $ opt_log_backup_number opt
      }
    logtype
      | not (opt_logging opt) = LogNone
      | debug                 = LogStdout $ naturalToInt logBufferSize
      | otherwise             = LogFile logspec $ naturalToInt logBufferSize

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

getTlsSetting :: Option -> IO TLSSettings
getTlsSetting _opt =
#ifdef HTTP_OVER_TLS
    case opt_service _opt of
        0 -> return defaultTlsSettings -- this is dummy
        _ -> do cert <- BS.readFile $ opt_tls_cert_file _opt
                let strip = dropWhileEnd isSpace . dropWhile isSpace
                    split "" = []
                    split s = case break (',' ==) s of
                      ("",r)  -> split (tail r)
                      (s',"") -> [s']
                      (s',r)  -> s' : split (tail r)
                    chain_files = map strip $ split $ opt_tls_chain_files _opt
                chains <- mapM BS.readFile chain_files
                key  <- BS.readFile $ opt_tls_key_file _opt
                let settings0 = tlsSettingsChainMemory cert chains key
                    settings = settings0 {
                        tlsSessionManagerConfig = Just defaultConfig { dbMaxSize = 1000 }
                      }
                return settings
#else
    return TLSSettings
#endif

----------------------------------------------------------------

ifRouteFileIsValid :: Reporter -> Option -> (RouteDB -> IO ()) -> IO ()
ifRouteFileIsValid rpt opt act = case opt_routing_file opt of
    Nothing    -> return ()
    Just rfile -> try (parseRoute rfile defaultDomain defaultPort) >>= either reportError act
  where
    reportError = report rpt . BS.pack . ioeGetErrorString

----------------------------------------------------------------

mighty :: Option -> Reporter -> Service
       -> ApacheLogger -> ServerPushLogger
       -> ConnPool -> RouteDBRef
       -> TLSSettings
       -> IO ()
mighty opt rpt svc lgr pushlgr mgr rdr _tlsSetting
  = reportDo rpt $ case svc of
    HttpOnly s  -> runSettingsSocket setting s app
#ifdef HTTP_OVER_TLS
    HttpsOnly s -> runTLSSocket _tlsSetting setting s app
    HttpAndHttps s1 s2 -> concurrently_
        (runSettingsSocket setting s1 app)
        (runTLSSocket _tlsSetting setting s2 app)
#ifdef HTTP_OVER_QUIC
    QUIC s1 s2 -> do
        smgr <- SM.newSessionManager SM.defaultConfig
        let settingT = setAltSvc "h3-27=\":4433\"" setting
        mapConcurrently_ id [runSettingsSocket        setting  s1 app
                            ,runTLSSocket _tlsSetting settingT s2 app
                            ,runQUIC (qconf smgr)     setting     app
                            ]
#endif
#else
    _ -> error "never reach"
#endif
  where
    app = P.pushOnReferer P.defaultSettings
                          (fileCgiApp cspec filespec cgispec revproxyspec rdr)
    debug = opt_debug_mode opt
    -- We don't use setInstallShutdownHandler because we may use
    -- two sockets for HTTP and HTTPS.
    setting = setPort            (naturalToInt $ opt_port opt) -- just in case
            $ setHost            (fromString (opt_host opt))  -- just in case
            $ setOnException     (if debug then printStdout else warpHandler rpt)
            $ setTimeout         (naturalToInt $ opt_connection_timeout opt) -- seconds
            $ setFdCacheDuration (naturalToInt $ opt_fd_cache_duration opt)
            $ setFileInfoCacheDuration 10
            $ setServerName      serverName
            $ setLogger          lgr
            $ setServerPushLogger pushlgr
            defaultSettings
    serverName = BS.pack $ opt_server_name opt
    cspec = ClassicAppSpec {
        softwareName = serverName
      , statusFileDir = fromString $ opt_status_file_dir opt
      }
    filespec = FileAppSpec {
        indexFile = fromString $ opt_index_file opt
      , isHTML = \x -> ".html" `isSuffixOf` x || ".htm" `isSuffixOf` x
      }
    cgispec = CgiAppSpec {
        indexCgi = "index.cgi"
      }
    revproxyspec = RevProxyAppSpec {
        revProxyManager = mgr
      }
#ifdef HTTP_OVER_QUIC
    qconf smgr = Q.defaultServerConfig {
            Q.scAddresses      = [("127.0.0.1",4433)]
          , Q.scKey            = opt_tls_key_file opt
          , Q.scCert           = opt_tls_cert_file opt
          , Q.scALPN           = Just chooseALPN
          , Q.scRequireRetry   = False
          , Q.scSessionManager = smgr
          , Q.scEarlyDataSize  = 1024
          , Q.scConfig     = Q.defaultConfig {
                Q.confParameters = Q.exampleParameters
              , Q.confDebugLog   = dirLogger (opt_quic_debug_dir opt) ".txt"
              , Q.confQLog       = dirLogger (opt_quic_qlog_dir opt) ".qlog"
              }
          }

chooseALPN :: Q.Version -> [ByteString] -> IO ByteString
chooseALPN ver protos
  | h3 `elem` protos = return h3
  | otherwise        = return ""
  where
    h3 = "h3-" `BS.append` BS.pack (show (Q.fromVersion ver))

dirLogger :: FilePath -> String -> (Q.CID -> String -> IO ())
dirLogger "" _ = \_ _ -> return ()
dirLogger dir suffix = \cid msg -> do
    let filename = BS.unpack (encode (Q.fromCID cid)) ++ suffix
        logfile = dir </> filename
    appendFile logfile msg `E.catch` \(E.SomeException _) -> do
        threadDelay 1000
        appendFile logfile msg
#endif

----------------------------------------------------------------

data Service = HttpOnly Socket
             | HttpsOnly Socket
             | HttpAndHttps Socket Socket
             | QUIC Socket Socket

----------------------------------------------------------------

openService :: Option -> IO Service
openService opt
  | service == 1 = do
      s <- bindPortTCP httpsPort hostpref
      debugMessage $ urlForHTTPS httpsPort
      return $ HttpsOnly s
  | service == 2 = do
      s1 <- bindPortTCP httpPort hostpref
      s2 <- bindPortTCP httpsPort hostpref
      debugMessage $ urlForHTTP httpPort
      debugMessage $ urlForHTTPS httpsPort
      return $ HttpAndHttps s1 s2
  | service == 3 = do
      s1 <- bindPortTCP httpPort hostpref
      s2 <- bindPortTCP httpsPort hostpref
      debugMessage $ urlForHTTP httpPort
      debugMessage $ urlForHTTPS httpsPort
      debugMessage $ "QUIC is also available via Alt-Svc"
      return $ QUIC s1 s2
  | otherwise = do
      s <- bindPortTCP httpPort hostpref
      debugMessage $ urlForHTTP httpPort
      return $ HttpOnly s
  where
    httpPort  = naturalToInt $ opt_port opt
    httpsPort = naturalToInt $ opt_tls_port opt
    hostpref  = fromString $ opt_host opt
    service = opt_service opt
    debug = opt_debug_mode opt
    debugMessage msg = when debug $ do
        putStrLn msg
        hFlush stdout
    urlForHTTP  80  =  "http://localhost/"
    urlForHTTP  p   =  "http://localhost:" ++ show p ++ "/"
    urlForHTTPS 443 = "https://localhost/"
    urlForHTTPS p   = "https://localhost:" ++ show p ++ "/"


----------------------------------------------------------------

closeService :: Service -> IO ()
closeService (HttpOnly s)         = close s
closeService (HttpsOnly s)        = close s
closeService (HttpAndHttps s1 s2) = close s1 >> close s2
closeService (QUIC s1 s2)         = close s1 >> close s2

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
      | otherwise                  = H.responseTimeoutMicro (naturalToInt $ opt_proxy_timeout opt * 1000000) -- micro seconds
