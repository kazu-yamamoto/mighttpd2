{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Applicative
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
    if opt_debug_mode opt
       then server opt route
       else daemonize $ server opt route
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
    installHandler sigCHLD Ignore Nothing
    unless debug writePidFile
    setGroupUser opt
    -- FIXME logging
    if preN == 1
       then server' opt route s
       else prefork opt route s
  where
    debug = opt_debug_mode opt
    port = opt_port opt
    sOpen = listenOn (PortNumber . fromIntegral $ port)
    pidfile = opt_pid_file opt
    preN = opt_prefork_process_number opt
    writePidFile = do
        pid <- getProcessID
        writeFile pidfile $ show pid ++ "\n"
        setFileMode pidfile 0o644
    handler :: SomeException -> IO ()
    handler e
      | debug = hPutStrLn stderr $ show e
      | otherwise = writeFile "/tmp/mighty_error" (show e)

server' :: Option -> RouteDB -> Socket -> IO ()
server' opt route s = do
    lgr <- if opt_logging opt
              then mightyLogger <$> if debug then stdoutInit
                                             else fileInit logspec
              else return (\_ _ _ -> return ())
    fif <- initialize
    runSettingsSocket setting s $ fileCgiApp (spec lgr fif) route
  where
    debug = opt_debug_mode opt
    setting = defaultSettings {
        settingsPort        = opt_port opt
      , settingsOnException = ignore
      , settingsTimeout     = opt_connection_timeout opt
      }
    spec lgr fif = AppSpec {
        softwareName = BS.pack $ opt_server_name opt
      , indexFile = BS.pack $ opt_index_file opt
      , isHTML = \x -> ".html" `BS.isSuffixOf` x || ".htm" `BS.isSuffixOf` x
      , logger = lgr
      , getFileInfo = fif
      }
    logspec = FileLogSpec {
        log_file          = opt_log_file opt
      , log_file_size     = fromIntegral $ opt_log_file_size opt
      , log_backup_number = opt_log_backup_number opt
      , log_buffer_size   = opt_log_buffer_size opt
      , log_flush_period  = opt_log_flush_period opt * 1000000
      }

prefork :: Option -> RouteDB -> Socket -> IO ()
prefork opt route s = do
    pid <- getProcessID
    cids <- replicateM preN $ forkProcess (server' opt route s)
    mapM_ (terminator pid cids) [sigTERM,sigINT]
    sClose s
    pause
  where
    terminator pid cids sig = installHandler sig (Catch (terminate pid cids)) Nothing
    terminate pid cids = do
        mapM_ terminateChild cids
        signalProcess killProcess pid
    terminateChild cid =  signalProcess sigTERM cid `catch` ignore
    preN = opt_prefork_process_number opt
    pause = threadDelay 5000000 >> pause

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
