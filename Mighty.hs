module Main where

import Config
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.List (isSuffixOf)
import FileCGIApp
import Log
import Network.Wai.Application.Classic
import Network.Wai.Handler.Warp (run)
import Route
import System.Environment
import System.Exit
import System.IO
import System.Posix

main :: IO ()
main = do
    opt  <- fileName 0 >>= parseOption
    route <- fileName 1 >>= parseRoute
    if opt_debug_mode opt
       then do
           chan <- setoutInit
           server chan opt route
       else daemonize $ do
           writePidFile opt
           chan <- fileInit (opt_log_file opt)
           server chan opt route
  where
    server chan opt route = do
        installHandler sigCHLD Ignore Nothing
        run (opt_port opt) $ \req -> do
            liftIO $ setGroupUser opt
            fileCgiApp spec route req
      where
        spec = AppSpec {
            softwareName = BS.pack $ opt_server_name opt
          , indexFile = opt_index_file opt
          , isHTML = \x -> ".html" `isSuffixOf` x || ".htm" `isSuffixOf` x
          , logger = mightyLogger chan
          }
    fileName n = do
        args <- getArgs
        when (length args /= 2) $ do
            hPutStrLn stderr "Usage: mighty config_file routing_file"
            exitFailure
        return $ args !! n
    writePidFile opt = do
        pid <- getProcessID
        writeFile (opt_pid_file opt) $ show pid ++ "\n"

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
