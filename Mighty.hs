module Main where

import Control.Monad.IO.Class (liftIO)
import Config
import Control.Monad
import FileCGIApp
import Network.Wai.Handler.Warp (run)
import System.Environment
import System.Exit
import System.IO
import System.Posix
import URLMap

main :: IO ()
main = do
    opt  <- fileName 0 >>= parseOption
    mapf <- fileName 1 >>= parseURLmap
    let server = run (opt_port opt) $ \req -> do
            liftIO $ setGroupUser opt
            fileCgiApp mapf req
    if opt_debug_mode opt
       then server
       else daemonize server
  where
    fileName n = do
        args <- getArgs
        when (length args /= 2) $ do
            hPutStrLn stderr "Usage: mighty config_file uri_map"
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
