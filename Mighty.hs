{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Applicative
import Control.Concurrent
import Control.Exception (try, handle, SomeException)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Conduit.Network
import FileCGIApp
import FileCache
import Network
import qualified Network.HTTP.Conduit as H
import Network.Wai.Application.Classic hiding ((</>))
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Logger.Prefork
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

----------------------------------------------------------------

main :: IO ()
main = do
    (opt,route) <- getOptRoute
    if opt_debug_mode opt then
        server opt route
      else do
        let port = opt_port opt
        putStrLn $ "Serving on port " ++ show port ++ " and detaching this terminal..."
        putStrLn $ "(If errors occur, they will be written in \"" ++ reportFile ++ "\".)"
        hFlush stdout
        daemonize $ server opt route
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

----------------------------------------------------------------

server :: Option -> RouteDB -> IO ()
server opt route = handle handler $ do
    s <- sOpen
    if debug then do
        putStrLn $ "Serving on port " ++ show port ++ "."
        hFlush stdout
      else
        writePidFile
    logCheck logtype
    sref <- initState
    myid <- getProcessID
    if workers == 1 then do
        void . forkIO $ single opt route s logtype sref -- killed by signal
        void . forkIO $ logController logtype [myid]
        slaveMainLoop sref
      else do
        cids <- multi opt route s logtype sref
        void . forkIO $ logController logtype cids
        masterMainLoop myid
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
    handler :: SomeException -> IO ()
    handler e
      | debug     = hPrint stderr e
      | otherwise = report $ BS.pack (show e)
    logspec = FileLogSpec {
        log_file          = opt_log_file opt
      , log_file_size     = fromIntegral $ opt_log_file_size opt
      , log_backup_number = opt_log_backup_number opt
      }
    logtype
      | not (opt_logging opt) = LogNone
      | debug                 = LogStdout
      | otherwise             = LogFile logspec

----------------------------------------------------------------

masterMainLoop :: ProcessID -> IO ()
masterMainLoop myid = do
    threadDelay 10000000
    cs <- findChildren myid
    if null cs then -- FIXME serverStatus st == Retiring
        return () -- FIXME
      else
        masterMainLoop myid

slaveMainLoop :: StateRef -> IO ()
slaveMainLoop sref = do
    threadDelay 1000000
    st <- getState sref
    if serverStatus st == Retiring && connectionCounter st == 0 then
        return () -- FIXME
      else
        slaveMainLoop sref

----------------------------------------------------------------

single :: Option -> RouteDB -> Socket -> LogType -> StateRef -> IO ()
single opt route s logtype sref = do
    setGroupUser opt -- don't change the user of the master process
    myThreadId >>= setWarpThreadId sref
    ignoreSigChild
    setHandler sigStop   stopHandler
    setHandler sigRetire retireHandler
    setHandler sigReload reloadHandler
    setHandler sigInfo   infoHandler
    lgr <- logInit FromSocket logtype
    getInfo <- fileCacheInit
    mgr <- H.newManager H.def {
            -- FIXME
            H.managerConnCount = 1024
          }
    runSettingsSocket setting s $ \req ->
        fileCgiApp (cspec lgr) (filespec getInfo) cgispec (revproxyspec mgr) route req
  where
    debug = opt_debug_mode opt
    setting = defaultSettings {
        settingsPort        = opt_port opt
      , settingsOnException = if debug then printStdout else ignore
      , settingsOnOpen      = increment sref
      , settingsOnClose     = decrement sref
      , settingsTimeout     = opt_connection_timeout opt
      , settingsHost        = HostAny
      }
    serverName = BS.pack $ opt_server_name opt
    cspec lgr = ClassicAppSpec {
        softwareName = serverName
      , logger = lgr
      , statusFileDir = fromString $ opt_status_file_dir opt
      }
    filespec getInfo = FileAppSpec {
        indexFile = fromString $ opt_index_file opt
      , isHTML = \x -> ".html" `isSuffixOf` x || ".htm" `isSuffixOf` x
      , getFileInfo = getInfo
      }
    cgispec = CgiAppSpec {
        indexCgi = "index.cgi"
      }
    revproxyspec mgr = RevProxyAppSpec {
        revProxyManager = mgr
      }
    stopHandler = Catch $ do
        sClose s
        exitImmediately ExitSuccess
    retireHandler = Catch $
        warpThreadId <$> getState sref >>>= \tid -> do
            killThread tid
            sClose s
            retireStatus sref
    reloadHandler = Catch $
        warpThreadId <$> getState sref >>>= \tid ->
        withRouteFile opt $ \newroute -> do
            killThread tid
            void . forkIO $ single opt newroute s logtype sref
    infoHandler = Catch $ do
        i <- BS.pack . show . connectionCounter <$> getState sref
        report $ "# of connections = " `BS.append` i

----------------------------------------------------------------

infixr 0 >>>=, >>>>

(>>>=) :: IO (Maybe a) -> (a -> IO ()) -> IO ()
x >>>= f = x >>= maybe (return ()) f

(>>>>) :: IO (Either IOError a) -> (a -> IO ()) -> IO ()
x >>>> f = bind x reportError f
  where
    bind y handler g = y >>= either handler g
    reportError = report . BS.pack . ioeGetErrorString

withRouteFile :: Option -> (RouteDB -> IO ()) -> IO ()
withRouteFile opt act =
    return (opt_routing_file opt)  >>>= \rfile ->
    try (parseRoute rfile)         >>>>
    act

----------------------------------------------------------------

multi :: Option -> RouteDB -> Socket -> LogType -> StateRef -> IO [ProcessID]
multi opt route s logtype sref = do
    ignoreSigChild
    cids <- replicateM workers $ forkProcess $ do
        void . forkIO $ single opt route s logtype sref -- killed by signal
        slaveMainLoop sref
    sClose s
    setHandler sigStop   $ stopHandler cids
    setHandler sigRetire $ retireHandler cids
    setHandler sigReload $ reloadHandler cids
    setHandler sigInfo   $ infoHandler cids
    return cids
  where
    workers = opt_worker_processes opt
    stopHandler cids   = Catch $ do
        mapM_ (sendSignal sigStop) cids
        exitImmediately ExitSuccess
    retireHandler cids = Catch $ do
        retireStatus sref
        mapM_ (sendSignal sigRetire) cids
    reloadHandler cids = Catch $ withRouteFile opt $ \_ ->
        mapM_ (sendSignal sigReload) cids
    infoHandler cids   = Catch $ mapM_ (sendSignal sigInfo) cids

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
        exitImmediately ExitSuccess
    ensureNeverAttachTerminal p = do
        void $ forkProcess p
        exitImmediately ExitSuccess
    detachTerminal = void createSession

----------------------------------------------------------------

printStdout :: SomeException -> IO ()
printStdout = print
