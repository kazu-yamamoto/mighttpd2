{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Applicative
import Control.Concurrent
import Control.Exception (catch, handle, SomeException)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Conduit.Network
import FileCGIApp
import FileCache
import Network
import qualified Network.HTTP.Conduit as H
import Network.Wai.Application.Classic
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Logger.Prefork
import Prelude hiding (catch)
import Route
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix
import Types

errorFile :: FilePath
errorFile = "/tmp/mighty_error"

main :: IO ()
main = do
    (opt,route) <- getOptRoute
    if opt_debug_mode opt then
        server opt route
      else do
        let port = opt_port opt
        putStrLn $ "Serving on port " ++ show port ++ " and detaching this terminal..."
        putStrLn $ "(If errors occur, they will be written in \"" ++ errorFile ++ "\".)"
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
          opt   <- parseOption $ args !! 0
          route <- parseRoute  $ args !! 1
          return (opt,route)
      | otherwise = do
          hPutStrLn stderr "Usage: mighty"
          hPutStrLn stderr "       mighty config_file routing_file"
          exitFailure
      where
        n = length args

----------------------------------------------------------------

server :: Option -> RouteDB -> IO ()
server opt route = handle handler $ do
    s <- sOpen
    if debug then do
        putStrLn $ "Serving on port " ++ show port ++ "."
        hFlush stdout
      else
        writePidFile
    setGroupUser opt
    logCheck logtype
    if workers == 1 then do
        _ <- forkIO $ single opt route s logtype -- killed by signal
        -- main thread
        myid <- getProcessID
        logController logtype [myid]
      else do
        cids <- multi opt route s logtype
        -- main thread
        logController logtype cids
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
      | debug = hPrint stderr e
      | otherwise = writeFile errorFile (show e)
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

single :: Option -> RouteDB -> Socket -> LogType -> IO ()
single opt route s logtype = do
    _ <- ignoreSigChild
    _ <- initHandler sigTERM $ Catch (sClose s >> exitImmediately ExitSuccess)
    _ <- initHandler sigINT  $ Catch (sClose s >> exitImmediately ExitSuccess)
    myid <- myThreadId
    _ <- initHandler sigQUIT $ Catch (killThread myid >> sClose s)
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

multi :: Option -> RouteDB -> Socket -> LogType -> IO [ProcessID]
multi opt route s logtype = do
    _ <- ignoreSigChild
    cids <- replicateM workers $ forkProcess $ do
        _ <- forkIO $ single opt route s logtype -- killed by signal
        -- main thread
        mainLoop
    sClose s
    _ <- initHandler sigTERM $ stopHandler cids
    _ <- initHandler sigINT  $ stopHandler cids
    _ <- initHandler sigQUIT $ quitHandler cids
    return cids
  where
    workers = opt_worker_processes opt
    stopHandler cids = Catch $ do
        mapM_ terminateChild cids
        exitImmediately ExitSuccess
    terminateChild cid = signalProcess sigTERM cid `catch` ignore
    quitHandler cids = Catch $ do
        mapM_ quitChild cids
    quitChild cid = signalProcess sigQUIT cid `catch` ignore
    mainLoop = threadDelay 1000000 >> mainLoop

initHandler :: Signal -> Handler -> IO Handler
initHandler sig func = installHandler sig func Nothing

ignoreSigChild :: IO Handler
ignoreSigChild = initHandler sigCHLD Ignore

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
    _ <- detachTerminal
    ensureNeverAttachTerminal $ do
        changeWorkingDirectory "/"
        _ <- setFileCreationMask 0
        mapM_ closeFd [stdInput, stdOutput, stdError]
        program
  where
    ensureDetachTerminalCanWork p = do
        _ <- forkProcess p
        exitImmediately ExitSuccess
    ensureNeverAttachTerminal p = do
        _ <- forkProcess p
        exitImmediately ExitSuccess
    detachTerminal = createSession

----------------------------------------------------------------

ignore :: SomeException -> IO ()
ignore _ = return ()

printStdout :: SomeException -> IO ()
printStdout = print
