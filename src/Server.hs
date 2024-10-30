{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Server (server, defaultDomain, defaultPort) where

import Control.Concurrent (runInUnboundThread)
import Control.Exception (try)
import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromRight)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Streaming.Network (bindPortTCP, bindPortUDP)
import GHC.Conc.Sync
import qualified Network.HTTP.Client as H
import Network.Socket (Socket, close)
import Network.Wai
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

#ifdef HTTP_OVER_TLS
import Control.Concurrent.Async (concurrently_)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Network.TLS (Credentials(..),SessionManager)
import qualified Network.TLS as TLS
import qualified Network.TLS.SessionTicket as SM
import Network.Wai.Handler.WarpTLS
#ifdef HTTP_OVER_QUIC
import Control.Concurrent.Async (mapConcurrently_)
import Data.Bits
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Network.QUIC.Internal as Q
import Network.Wai.Handler.WarpQUIC
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
    labelMe "Mighty main"
    unlimit openFileNumber
    svc <- openService opt
    unless debug writePidFile
    rdr <- newRouteDBRef route
    tmgr <- T.initialize (naturalToInt (opt_connection_timeout opt) * 1000000)
    (mcred, smgr) <- setup opt
    _changed <- setGroupUser (opt_user opt) (opt_group opt)
    logCheck logtype
    -- "Time cacher of FastLogger (AutoUpdate)"
    (zdater,_) <- clockDateCacher
    -- Loggerset of FastLogger (Debounce)
    ap <- initLogger FromSocket logtype zdater
    let lgr = apacheLogger ap
        remover = logRemover ap
        pushlgr = serverpushLogger ap
    -- HTTP Client Manager
    mgr <- getManager opt
    setHandlers opt rpt svc remover rdr

    report rpt "Mighty started"
    runInUnboundThread $ do
        labelMe "Mighty main (bound thread)"
        mighty opt rpt svc lgr pushlgr mgr rdr mcred smgr tmgr
    report rpt "Mighty retired"
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
        closeService svc
        remover
        exitImmediately ExitSuccess
    retireHandler = Catch $ do
        report rpt "Mighty retiring"
        closeService svc -- this lets warp break
    infoHandler = Catch $ do
        labelMe "Info signale handler"
        threadSummary >>= mapM_ (putStrLn . showT)
    showT (i, l, s) = i ++ " " ++ l ++ ": " ++ show s
    reloadHandler = Catch $ do
        ifRouteFileIsValid rpt opt $ \newroute -> do
            writeRouteDBRef rdr newroute
            report rpt "Mighty reloaded"

threadSummary :: IO [(String, String, ThreadStatus)]
threadSummary = (sort <$> listThreads) >>= mapM summary
  where
    summary t = do
        let idstr = drop 9 $ show t
        l <- fromMaybe "(no name)" <$> threadLabel t
        s <- threadStatus t
        return (idstr, l, s)

#ifdef HTTP_OVER_TLS
loadCredentials :: Option -> IO Credentials
loadCredentials opt = do
    cert   <- BS.readFile $ opt_tls_cert_file opt
    chains <- mapM BS.readFile chain_files
    key    <- BS.readFile $ opt_tls_key_file opt
    let cred = fromRight (error "loadCredentials") $ TLS.credentialLoadX509ChainFromMemory cert chains key
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
    Just rfile -> try (parseRoute rfile defaultDomain defaultPort) >>= either reportError_ act
  where
    reportError_ = report rpt . BS.pack . ioeGetErrorString

----------------------------------------------------------------

mighty :: Option -> Reporter -> Service
       -> ApacheLogger -> ServerPushLogger
       -> ConnPool -> RouteDBRef
       -> Maybe Credentials -> Maybe SessionManager -> T.Manager
       -> IO ()
mighty opt rpt svc lgr pushlgr mgr rdr _mcreds _msmgr tmgr
  = reportDo rpt $ case svc of
    HttpOnly s  -> runHTTP setting s app
#ifdef HTTP_OVER_TLS
    HttpsOnly s -> runHTTPS tlsSetting setting s app
    HttpAndHttps s1 s2 -> concurrently_
        (runHTTP setting s1 app)
        (runHTTPS tlsSetting setting s2 app)
#ifdef HTTP_OVER_QUIC
    QUIC s1 s2 ss3 -> do
        let quicPort' = BS.pack $ show quicPort
            strver Q.Version1 = ""
            strver Q.Version2 = ""
            strver v = BS.append "-" $ BS.pack $ show $ fromVersion v
            quicDrafts = map strver quicVersions
            value v = BS.concat ["h3",v,"=\":",quicPort',"\""]
            altsvc = BS.intercalate "," $ map value quicDrafts
            settingT = setAltSvc altsvc setting
            h12  = concurrently_ (runHTTP             setting  s1 app)
                                 (runHTTPS tlsSetting settingT s2 app)
            h123 = concurrently_ (runHTTP3 qconf      setting  ss3 app)
                                 (labelMe "concurrently" >> h12)
        h123
#else
    _ -> error "never reach"
#endif
#else
    _ -> error "never reach"
#endif
  where
    app = fileCgiApp cspec filespec cgispec revproxyspec rdr
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
    ~tlsSetting = defaultTlsSettings {
        tlsCredentials    = _mcreds
      , tlsSessionManager = _msmgr
      , tlsAllowedVersions = [TLS.TLS13,TLS.TLS12]
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
    ~quicAddr = read <$> opt_quic_addr opt
    ~quicPort = fromIntegral $ opt_quic_port opt
    ~quicVersions = Q.scVersions Q.defaultServerConfig
    -- Lazy binding for opt_service == 0 so that
    -- 'fromJust' is not called.
    ~qconf = Q.defaultServerConfig {
            Q.scAddresses      = (,quicPort) <$> quicAddr
          , Q.scALPN           = Just chooseALPN
          , Q.scRequireRetry   = False
          , Q.scSessionManager = fromJust _msmgr
          , Q.scUse0RTT        = True
          , Q.scDebugLog       = opt_quic_debug_dir opt
          , Q.scQLog           = opt_quic_qlog_dir opt
          , Q.scCredentials    = fromJust _mcreds
          }

chooseALPN :: Q.Version -> [ByteString] -> IO ByteString
chooseALPN ver protos = case find (\x -> x == h3 || x == hq) protos of
  Nothing    -> return ""
  Just proto -> return proto
  where
    h3 | ver == Q.Version1 = "h3"
       | ver == Q.Version2 = "h3"
       | otherwise = "h3-" `BS.append` BS.pack (show (fromVersion ver))
    hq | ver == Q.Version1 = "hq-interop"
       | ver == Q.Version2 = "hq-interop"
       | otherwise = "hq-" `BS.append` BS.pack (show (fromVersion ver))

fromVersion :: Q.Version -> Int
fromVersion (Q.Version ver) = fromIntegral (0x000000ff .&. ver)
#endif

----------------------------------------------------------------

data Service = HttpOnly Socket
             | HttpsOnly Socket
             | HttpAndHttps Socket Socket
             | QUIC Socket Socket [Socket]

instance Show Service where
    show HttpOnly{}     = "HttpOnly"
    show HttpsOnly{}    = "HttpOnlys"
    show HttpAndHttps{} = "HttpAndHttps"
    show QUIC{}         = "QUIC"

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
      ss3 <- mapM (bindPortUDP quicPort) quicAddrs
      debugMessage $ urlForHTTP httpPort
      debugMessage $ urlForHTTPS httpsPort
      debugMessage "QUIC is also available via Alt-Svc"
      return $ QUIC s1 s2 ss3
  | otherwise = do
      s <- bindPortTCP httpPort hostpref
      debugMessage $ urlForHTTP httpPort
      return $ HttpOnly s
  where
    httpPort  = naturalToInt $ opt_port opt
    httpsPort = naturalToInt $ opt_tls_port opt
    quicPort = naturalToInt $ opt_quic_port opt
    quicAddrs = fromString <$> opt_quic_addr opt
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
closeService (QUIC s1 s2 ss3)     = close s1 >> close s2 >> mapM_ close ss3

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

----------------------------------------------------------------

setup :: Option -> IO (Maybe Credentials, Maybe SessionManager)
#ifdef HTTP_OVER_TLS
setup opt
  | 1 <= service && service <= 3 = do
        mcred <- Just <$> loadCredentials opt
        smgr <- Just <$> SM.newSessionTicketManager SM.defaultConfig
        return (mcred, smgr)
  | otherwise = return (Nothing, Nothing)
  where
    service = opt_service opt
#else
setup _ = return (Nothing, Nothing)
#endif

labelMe :: String -> IO ()
labelMe lbl = do
    tid <- myThreadId
    labelThread tid lbl

runHTTP :: Settings -> Socket -> Application -> IO ()
runHTTP setting s app = do
    labelMe "HTTP1/2 server"
    runSettingsSocket setting s app

#ifdef HTTP_OVER_TLS
runHTTPS :: TLSSettings -> Settings -> Socket -> Application -> IO ()
runHTTPS tlsSetting setting s app = do
    labelMe "HTTP1/2 over TLS server"
    runTLSSocket tlsSetting setting s app

#ifdef HTTP_OVER_QUIC
runHTTP3 :: QUICSettings -> Settings -> [Socket]-> Application -> IO ()
runHTTP3 qconf setting ss app = do
    labelMe "HTTP3 server"
    runQUICSockets qconf setting ss app
#endif
#endif
