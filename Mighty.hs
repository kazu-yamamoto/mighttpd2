module Main where

import Config
import Control.Exception (handle, SomeException)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List (isSuffixOf)
import FileCGIApp
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
    lgr <- if opt_logging opt
              then do
               chan <- if debug then stdoutInit else fileInit logspec
               return $ mightyLogger chan
              else return (\_ _ _ -> return ())
    runSettingsSocket setting s $ fileCgiApp (spec lgr) route
  where
    debug = opt_debug_mode opt
    port = opt_port opt
    ignore = const $ return ()
    sOpen = listenOn (PortNumber . fromIntegral $ port)
    spec lgr = AppSpec {
        softwareName = BS.pack $ opt_server_name opt
      , indexFile = opt_index_file opt
      , isHTML = \x -> ".html" `isSuffixOf` x || ".htm" `isSuffixOf` x
      , logger = lgr
      }
    logspec = FileLogSpec {
        log_file          = opt_log_file opt
      , log_file_size     = fromIntegral $ opt_log_file_size opt
      , log_backup_number = opt_log_backup_number opt
      , log_buffer_size   = opt_log_buffer_size opt
      , log_flush_period  = opt_log_flush_period opt * 1000000
      }
    setting = defaultSettings {
        settingsPort        = opt_port opt
      , settingsOnException = ignore
      , settingsTimeout     = opt_connection_timeout opt
      }
    pidfile = opt_pid_file opt
    writePidFile = do
        pid <- getProcessID
        writeFile pidfile $ show pid ++ "\n"
        setFileMode pidfile 0o644
    handler :: SomeException -> IO ()
    handler e
      | debug = hPutStrLn stderr $ show e
      | otherwise = writeFile "/tmp/mighty_error" (show e)

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
