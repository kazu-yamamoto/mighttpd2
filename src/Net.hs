module Net (listenSocket) where

import Control.Exception
import Network
import Network.BSD
import Network.Socket as NS

listenSocket :: String -> IO Socket
listenSocket serv = do
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]
                             , addrSocketType = Stream
                             , addrProtocol = proto }
    addrs <- getAddrInfo (Just hints) Nothing (Just serv)
    let addrs' = filter (\x -> addrFamily x == AF_INET6) addrs
        addr = head $ if null addrs' then addrs else addrs'
    listenSocket' addr

listenSocket' :: AddrInfo -> IO Socket
listenSocket' addr = bracketOnError setup cleanup $ \sock -> do
    setSocketOption sock ReuseAddr 1
    setSocketOption sock NoDelay 1
    bindSocket sock (addrAddress addr)
    listen sock 2048
    return sock
 where
   setup = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
   cleanup = NS.sClose
