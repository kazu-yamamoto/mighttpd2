module Main where

import Config
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import FileCGIApp
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
    let server = run (opt_port opt) $ \req -> do
            liftIO $ setGroupUser opt
            fileCgiApp opt route req
    if opt_debug_mode opt
       then server
       else daemonize server
  where
    fileName n = do
        args <- getArgs
        when (length args /= 2) $ do
            hPutStrLn stderr "Usage: mighty config_file routing_file"
            exitFailure
        return $ args !! n

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
