module Program.Mighty.Network (
    listenSocket
  , daemonize
  ) where

import Control.Exception
import Control.Monad
import Network
import Network.BSD
import Network.Socket
import System.Exit
import System.Posix

----------------------------------------------------------------

-- | Open an 'Socket' with 'ReuseAddr' and 'NoDelay' set.
listenSocket :: String -- ^ Service name
             -> Int    -- ^ A number of backlogs.
             -> IO Socket
listenSocket serv backlog = do
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]
                             , addrSocketType = Stream
                             , addrProtocol = proto }
    addrs <- getAddrInfo (Just hints) Nothing (Just serv)
    let addrs' = filter (\x -> addrFamily x == AF_INET6) addrs
        addr = head $ if null addrs' then addrs else addrs'
    listenSocket' addr backlog

listenSocket' :: AddrInfo -> Int -> IO Socket
listenSocket' addr backlog = bracketOnError setup cleanup $ \sock -> do
    setSocketOption sock ReuseAddr 1
    setSocketOption sock NoDelay 1
    bindSocket sock (addrAddress addr)
    listen sock backlog
    return sock
 where
   setup = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
   cleanup = sClose

----------------------------------------------------------------

-- | Run a program detaching its terminal.
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
