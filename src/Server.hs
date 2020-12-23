{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Server (server, defaultDomain, defaultPort) where

import Control.Concurrent (runInUnboundThread)
import Control.Exception (try)
import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as BS
import Data.Streaming.Network (bindPortTCP)
import qualified Network.HTTP.Client as H
import Network.Socket (Socket, close)
import Network.Wai.Application.Classic hiding ((</>))
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import System.Exit (ExitCode(..), exitSuccess)
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Posix (exitImmediately, Handler(..), getProcessID, setFileMode)
import System.Posix.Signals (sigCHLD)
import qualified System.TimeManager as T

import Program.Mighty
import WaiApp

import qualified Network.Wai.Middleware.Push.Referer as P

#ifdef HTTP_OVER_TLS
import Control.Concurrent.Async (concurrently_)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Network.TLS (Credentials(..),SessionManager)
import qualified Network.TLS as TLS
import Network.TLS.SessionManager
import qualified Network.TLS.SessionManager as SM
import Network.Wai.Handler.WarpTLS
#ifdef HTTP_OVER_QUIC
import Control.Concurrent.Async (mapConcurrently_)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Network.QUIC as Q
import Network.Wai.Handler.WarpQUIC
#ifdef DROP_EXCEPT_BIND
import Control.Monad (forM_)
import Foreign.C.Types (CInt(..))
import System.Directory (listDirectory)
import System.Posix.Signals (sigUSR1)
#endif
#endif
#else
data Credentials
data SessionManager
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
    tmgr <- T.initialize (naturalToInt (opt_connection_timeout opt) * 1000000)
#ifdef HTTP_OVER_TLS
    mcred <- Just <$> loadCredentials opt
    smgr <- Just <$> SM.newSessionManager SM.defaultConfig { dbMaxSize = 1000 }
#else
    let mcred = Nothing
        smgr = Nothing
#endif
    _changed <- setGroupUser (opt_user opt) (opt_group opt)
#ifdef DROP_EXCEPT_BIND
    when _changed dropExceptBind
#endif
    logCheck logtype
    (zdater,_) <- clockDateCacher
    ap <- initLogger FromSocket logtype zdater
    let lgr = apacheLogger ap
        remover = logRemover ap
        pushlgr = serverpushLogger ap
    mgr <- getManager opt
    setHandlers opt rpt svc remover rdr

    report rpt "Mighty started"
    runInUnboundThread $ mighty opt rpt svc lgr pushlgr mgr rdr mcred smgr tmgr
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
    infoHandler = Catch $ report rpt "obsoleted"
    reloadHandler = Catch $ do
        ifRouteFileIsValid rpt opt $ \newroute -> do
            writeRouteDBRef rdr newroute
            report rpt "Mighty reloaded"

#ifdef HTTP_OVER_TLS
loadCredentials :: Option -> IO Credentials
loadCredentials opt = do
    cert   <- BS.readFile $ opt_tls_cert_file opt
    chains <- mapM BS.readFile chain_files
    key    <- BS.readFile $ opt_tls_key_file opt
    let Right cred = TLS.credentialLoadX509ChainFromMemory cert chains key
    return $ Credentials [cred]
  where
    strip = dropWhileEnd isSpace . dropWhile isSpace
    split "" = []
    split s = case break (',' ==) s of
      ("",r)  -> split (tail r)
      (s',"") -> [s']
      (s',r)  -> s' : split (tail r)
    chain_files = map strip $ split $ opt_tls_chain_files opt
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
       -> Maybe Credentials -> Maybe SessionManager -> T.Manager
       -> IO ()
mighty opt rpt svc lgr pushlgr mgr rdr _mcreds _msmgr tmgr
  = reportDo rpt $ case svc of
    HttpOnly s  -> runSettingsSocket setting s app
#ifdef HTTP_OVER_TLS
    HttpsOnly s -> runTLSSocket tlsSetting setting s app
    HttpAndHttps s1 s2 -> concurrently_
        (runSettingsSocket setting s1 app)
        (runTLSSocket tlsSetting setting s2 app)
#ifdef HTTP_OVER_QUIC
    QUIC s1 s2 -> do
        let quicPort' = BS.pack $ show quicPort
            strver Q.Version1 = ""
            strver v = BS.append "-" $ BS.pack $ show $ Q.fromVersion v
            quicDrafts = map strver quicVersions
            value v = BS.concat ["h3",v,"=\":",quicPort',"\""]
            altsvc = BS.intercalate "," $ map value quicDrafts
            settingT = setAltSvc altsvc setting
        mapConcurrently_ id [runSettingsSocket        setting  s1 app
                            ,runTLSSocket  tlsSetting settingT s2 app
                            ,runQUIC       qconf      setting     app
                            ]
#else
    _ -> error "never reach"
#endif
#else
    _ -> error "never reach"
#endif
  where
    app = P.pushOnReferer P.defaultSettings $ fileCgiApp cspec filespec cgispec revproxyspec rdr
    debug = opt_debug_mode opt
    -- We don't use setInstallShutdownHandler because we may use
    -- two sockets for HTTP and HTTPS.
    setting = setPort            (naturalToInt $ opt_port opt) -- just in case
            $ setHost            (fromString (opt_host opt))  -- just in case
            $ setOnException     (if debug then printStdout else warpHandler rpt)
            $ setManager         tmgr
            $ setFdCacheDuration (naturalToInt $ opt_fd_cache_duration opt)
            $ setFileInfoCacheDuration 10
            $ setServerName      serverName
            $ setLogger          lgr
            $ setServerPushLogger pushlgr
            defaultSettings
#ifdef HTTP_OVER_TLS
    tlsSetting = defaultTlsSettings {
        tlsCredentials    = _mcreds
      , tlsSessionManager = _msmgr
      }
#endif
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
    quicAddr = read <$> opt_quic_addr opt
    quicPort = fromIntegral $ opt_quic_port opt
    quicVersions = Q.confVersions $ Q.defaultConfig
    qconf = Q.defaultServerConfig {
            Q.scAddresses      = (,quicPort) <$> quicAddr
          , Q.scALPN           = Just chooseALPN
          , Q.scRequireRetry   = False
          , Q.scSessionManager = fromJust _msmgr
          , Q.scEarlyDataSize  = 1024
          , Q.scDebugLog       = opt_quic_debug_dir opt
          , Q.scConfig     = (Q.scConfig Q.defaultServerConfig) {
                Q.confQLog        = opt_quic_qlog_dir opt
              , Q.confCredentials = fromJust _mcreds
              }
          }

chooseALPN :: Q.Version -> [ByteString] -> IO ByteString
chooseALPN ver protos = case find (\x -> x == h3 || x == hq) protos of
  Nothing    -> return ""
  Just proto -> return proto
  where
    h3 | ver == Q.Version1 = "h3"
       | otherwise = "h3-" `BS.append` BS.pack (show (Q.fromVersion ver))
    hq | ver == Q.Version1 = "hq-interop"
       | otherwise = "hq-" `BS.append` BS.pack (show (Q.fromVersion ver))
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
      debugMessage "QUIC is also available via Alt-Svc"
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

#ifdef DROP_EXCEPT_BIND
foreign import ccall unsafe "send_signal"
  c_send_signal :: CInt -> CInt -> IO ()

dropExceptBind :: IO ()
dropExceptBind = do
    pid <- getProcessID
    strtids <- listDirectory ("/proc/" ++ show pid ++ "/task")
    let tids = map read strtids :: [Int]
    forM_ tids $ \tid -> c_send_signal (fromIntegral tid) sigUSR1
#endif
