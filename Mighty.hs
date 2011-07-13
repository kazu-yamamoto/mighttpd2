{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Main where

import Config
import Control.Concurrent
import Control.Exception (catch, handle, SomeException)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import FileCGIApp
import FileCache
import Log
import Network
import Network.Wai.Application.Classic
import Network.Wai.Handler.Warp
import Prelude hiding (catch)
import Route
import System.Environment
import System.Exit
import System.IO
import System.Posix
import Types

main :: IO ()
main = do
    opt  <- fileName 0 >>= parseOption
    route <- fileName 1 >>= parseRoute
    if opt_debug_mode opt then
        server opt route
    else
        daemonize $ server opt route
  where
    fileName n = do
        args <- getArgs
        when (length args /= 2) $ do
            hPutStrLn stderr "Usage: mighty config_file routing_file"
            exitFailure
        return $ args !! n

server :: Option -> RouteDB -> IO ()
server opt route = handle handler $ do
    s <- sOpen
    unless debug writePidFile
    setGroupUser opt
    if workers == 1 then do
        single opt route s logspec
--        fileRotater logspec [pid]
    else do
        multi opt route s logspec
--        fileRotater logspec [pid]
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
      | debug = hPutStrLn stderr $ show e
      | otherwise = writeFile "/tmp/mighty_error" (show e)
    logspec = FileLogSpec {
        log_file          = opt_log_file opt
      , log_file_size     = fromIntegral $ opt_log_file_size opt
      , log_backup_number = opt_log_backup_number opt
      }

single :: Option -> RouteDB -> Socket -> FileLogSpec -> IO ()
single opt route s logspec = do
    lgr <- if opt_logging opt then do
               logInit logspec
           else
               return (\_ _ _ -> return ())
    getInfo <- fileCacheInit
    runSettingsSocket setting s $ fileCgiApp (spec lgr getInfo) route
  where
--    debug = opt_debug_mode opt
    setting = defaultSettings {
        settingsPort        = opt_port opt
      , settingsOnException = ignore
      , settingsTimeout     = opt_connection_timeout opt
      }
    spec lgr getInfo = AppSpec {
        softwareName = BS.pack $ opt_server_name opt
      , indexFile = BS.pack $ opt_index_file opt
      , isHTML = \x -> ".html" `BS.isSuffixOf` x || ".htm" `BS.isSuffixOf` x
      , logger = lgr
      , getFileInfo = getInfo
      }

multi :: Option -> RouteDB -> Socket -> FileLogSpec -> IO ()
multi opt route s logspec = do
    ignoreSigChild
    cids <- replicateM workers $ forkProcess (single opt route s logspec)
    sClose s
    initHandler sigTERM $ terminateHandler cids
    initHandler sigINT  $ terminateHandler cids
    forever $ threadDelay 10000000
    return ()
--    fileRotater logspec cids
  where
    workers = opt_worker_processes opt
    initHandler sig func = installHandler sig func Nothing
    ignoreSigChild = initHandler sigCHLD Ignore
    terminateHandler cids = Catch $ do
        mapM_ terminateChild cids
        exitImmediately ExitSuccess
    terminateChild cid = signalProcess sigTERM cid `catch` ignore

----------------------------------------------------------------

setGroupUser :: Option -> IO ()
setGroupUser opt = do
    uid <- getRealUserID
    when (uid == 0) $ do
        getGroupEntryForName (opt_group opt) >>= setGroupID . groupID
        getUserEntryForName (opt_user opt) >>= setUserID . userID

----------------------------------------------------------------

daemonize :: IO () -> IO ()
daemonize program = ensureDetachTerminalCanWork $ do
    detachTerminal
    ensureNeverAttachTerminal $ do
        changeWorkingDirectory "/"
        setFileCreationMask 0
        mapM_ closeFd [stdInput, stdOutput, stdError]
        program
  where
    ensureDetachTerminalCanWork p = do
        forkProcess p
        exitImmediately ExitSuccess
    ensureNeverAttachTerminal p = do
        forkProcess p
        exitImmediately ExitSuccess
    detachTerminal = createSession

----------------------------------------------------------------

ignore :: SomeException -> IO ()
ignore = const $ return ()
