{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Conduit.Network
import FileCGIApp
import FileCache
import Network
import qualified Network.HTTP.Conduit as H
import Network.Wai.Application.Classic hiding ((</>), (+++))
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Logger.Prefork
import Prelude hiding (catch)
import Process
import Report
import Route
import Signal
import State
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Posix
import Types
import Utils

----------------------------------------------------------------

main :: IO ()
main = do
    (opt,route) <- getOptRoute
    rpt <- mkReporter >>= checkReporter
    if opt_debug_mode opt then
        server opt route rpt
      else
        background opt $ server opt route rpt
  where
    getOptRoute = getArgs >>= eachCase
    eachCase args
      | n == 0 = do
          root <- amIrootUser
          let opt | root      = defaultOption { opt_port = 80 }
                  | otherwise = defaultOption
          dir <- getCurrentDirectory
          let dst = fromString . addTrailingPathSeparator $ dir
              route = [Block ["*"] [RouteFile "/" dst]]
          return (opt, route)
      | n == 2 = do
          let config_file = args !! 0
          routing_file <- getAbsoluteFile (args !! 1)
          opt   <- parseOption config_file
          route <- parseRoute  routing_file
          let opt' = opt {opt_routing_file = Just routing_file}
          return (opt',route)
      | otherwise = do
          hPutStrLn stderr "Usage: mighty"
          hPutStrLn stderr "       mighty config_file routing_file"
          exitFailure
      where
        n = length args
    getAbsoluteFile file
        | isAbsolute file = return file
        | otherwise       = do
            dir <- getCurrentDirectory
            return $ dir </> normalise file
    checkReporter (Right rpt) = return rpt
    checkReporter (Left e)    = do
        hPutStrLn stderr $ reportFile ++ " is not writable"
        hPrint stderr e
        exitFailure

----------------------------------------------------------------

server :: Option -> RouteDB -> Reporter -> IO ()
server opt route rpt = reportDo rpt $ do
    unlimit
    s <- sOpen
    if debug then do
        putStrLn $ "Serving on port " ++ show port ++ "."
        hFlush stdout
      else
        writePidFile
    logCheck logtype
    myid <- getProcessID
    sref <- initState
    if workers == 1 then do
        void . forkIO $ single opt route s logtype sref rpt -- killed by signal
        void . forkIO $ logController logtype [myid]
        slaveMainLoop rpt sref
      else do
        cids <- multi opt route s logtype sref rpt
        void . forkIO $ logController logtype cids
        masterMainLoop rpt myid
  where
    debug = opt_debug_mode opt
    port = opt_port opt
    sOpen = listenOn (PortNumber . fromIntegral $ port)
    pidfile = opt_pid_file opt
    workers = opt_worker_processes opt
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
      | debug                 = LogStdout
      | otherwise             = LogFile logspec sigLogCtl

----------------------------------------------------------------

masterMainLoop :: Reporter -> ProcessID -> IO ()
masterMainLoop rpt myid = do
    threadDelay 10000000
    cs <- findChildren myid
    if null cs then do -- FIXME serverStatus st == Retiring
        -- FIXME
        report rpt "Master Mighty retired"
        closeReporter rpt
        exitSuccess
      else
        masterMainLoop rpt myid

slaveMainLoop :: Reporter -> StateRef -> IO ()
slaveMainLoop rpt sref = do
    threadDelay 1000000
    st <- getState sref
    if serverStatus st == Retiring && connectionCounter st == 0 then do
        -- FIXME
        report rpt "Worker Mighty retired"
        closeReporter rpt
        exitSuccess
      else
        slaveMainLoop rpt sref

----------------------------------------------------------------

reportDo :: Reporter -> IO () -> IO ()
reportDo rpt act = act `catch` warpHandler rpt

warpHandler :: Reporter -> SomeException -> IO ()
warpHandler rpt e = do
    let ah :: AsyncException -> IO ()
        ah ThreadKilled = return ()
        ah x            = report rpt $ bshow x
        ih :: InvalidRequest -> IO ()
        ih _            = return ()
        sh :: SomeException -> IO ()
        sh x            = report rpt $ bshow x
    throwIO e `catches` [Handler ah, Handler ih, Handler sh]

----------------------------------------------------------------

single :: Option -> RouteDB -> Socket -> LogType -> StateRef -> Reporter -> IO ()
single opt route s logtype sref rpt = reportDo rpt $ do
    setGroupUser opt -- don't change the user of the master process
    ignoreSigChild
    lgr <- logInit FromSocket logtype
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
    single' opt route s sref rpt lgr getInfo mgr
  where
    stopHandler = Catch $ do
        report rpt "Worker Mighty finished"
        closeReporter rpt
        -- FIXME
        sClose s
        exitImmediately ExitSuccess
    retireHandler = Catch $
        warpThreadId <$> getState sref >>>= \tid -> do
            report rpt "Worker Mighty retiring"
            killThread tid
            sClose s
            retireStatus sref
    reloadHandler lgr getInfo mgr = Catch $
        warpThreadId <$> getState sref >>>= \tid ->
        ifRouteFileIsValid rpt opt $ \newroute -> do
            report rpt "Worker Mighty reloaded"
            killThread tid
            void . forkIO $ single' opt newroute s sref rpt lgr getInfo mgr
    infoHandler = Catch $ do
        st <- getState sref
        let i =  bshow $ connectionCounter st
            status = bshow $ serverStatus st
        report rpt $ status +++ ": # of connections = " +++ i

single' :: Option -> RouteDB -> Socket
        -> StateRef -> Reporter -> ApacheLogger 
        -> (Path -> IO FileInfo) -> H.Manager
        -> IO ()
single' opt route s sref rpt lgr getInfo mgr = reportDo rpt $ do
    myThreadId >>= setWarpThreadId sref
    runSettingsSocket setting s $ \req ->
        fileCgiApp cspec filespec cgispec revproxyspec route req
  where
    debug = opt_debug_mode opt
    setting = defaultSettings {
        settingsPort        = opt_port opt
      , settingsOnException = if debug then printStdout else warpHandler rpt
      , settingsOnOpen      = increment sref
      , settingsOnClose     = decrement sref
      , settingsTimeout     = opt_connection_timeout opt
      , settingsHost        = HostAny
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
        revProxyManager = mgr
      }

----------------------------------------------------------------

multi :: Option -> RouteDB -> Socket -> LogType -> StateRef -> Reporter -> IO [ProcessID]
multi opt route s logtype sref rpt = do
    report rpt "Master Mighty started"
    ignoreSigChild
    cids <- replicateM workers $ forkProcess $ do
        void . forkIO $ single opt route s logtype sref rpt -- killed by signal
        slaveMainLoop rpt sref
    sClose s
    setHandler sigStop   $ stopHandler cids
    setHandler sigINT    $ stopHandler cids -- C-c from keyboard when debugging
    setHandler sigRetire $ retireHandler cids
    setHandler sigReload $ reloadHandler cids
    setHandler sigInfo   $ infoHandler cids
    return cids
  where
    workers = opt_worker_processes opt
    stopHandler cids   = Catch $ do
        report rpt "Master Mighty finished"
        closeReporter rpt
        mapM_ (sendSignal sigStop) cids
        -- FIXME
        exitImmediately ExitSuccess
    retireHandler cids = Catch $ do
        report rpt "Master Mighty retiring"
        retireStatus sref
        mapM_ (sendSignal sigRetire) cids
    reloadHandler cids = Catch $ ifRouteFileIsValid rpt opt $ \_ -> do
        report rpt "Master Mighty reloaded"
        mapM_ (sendSignal sigReload) cids
    infoHandler cids   = Catch $ mapM_ (sendSignal sigInfo) cids

----------------------------------------------------------------

ifRouteFileIsValid :: Reporter -> Option -> (RouteDB -> IO ()) -> IO ()
ifRouteFileIsValid rpt opt act =
    return (opt_routing_file opt) >>>= \rfile ->
    try (parseRoute rfile) >>= either reportError act
  where
    reportError = report rpt . BS.pack . ioeGetErrorString

----------------------------------------------------------------

amIrootUser :: IO Bool
amIrootUser = (== 0) <$> getRealUserID

setGroupUser :: Option -> IO ()
setGroupUser opt = do
    root <- amIrootUser
    when root $ do
        getGroupEntryForName (opt_group opt) >>= setGroupID . groupID
        getUserEntryForName (opt_user opt) >>= setUserID . userID

----------------------------------------------------------------

unlimit :: IO ()
unlimit = handle ignore $ do
    hard <- hardLimit <$> getResourceLimit ResourceOpenFiles
    let lim = if hard == ResourceLimitInfinity then
                  ResourceLimits (ResourceLimit 10000) hard
                else
                  ResourceLimits hard hard
    setResourceLimit ResourceOpenFiles lim

----------------------------------------------------------------

background :: Option -> IO () -> IO ()
background opt svr = do
    putStrLn $ "Serving on port " ++ show port ++ " and detaching this terminal..."
    putStrLn $ "(If errors occur, they will be written in \"" ++ reportFile ++ "\".)"
    hFlush stdout
    daemonize svr
  where
    port = opt_port opt

daemonize :: IO () -> IO ()
daemonize program = ensureDetachTerminalCanWork $ do
    detachTerminal
    ensureNeverAttachTerminal $ do
        changeWorkingDirectory "/"
        void $ setFileCreationMask 0
        mapM_ closeFd [stdInput, stdOutput, stdError]
        program
  where
    ensureDetachTerminalCanWork p = do
        void $ forkProcess p
        exitSuccess
    ensureNeverAttachTerminal p = do
        void $ forkProcess p
        exitSuccess
    detachTerminal = void createSession
