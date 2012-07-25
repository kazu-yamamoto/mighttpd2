module Log (
    Logger
  , initLogger
  , apatcheLogger
  , finLogger
  ) where

import Network.Wai.Logger
import Network.Wai.Logger.Prefork

data Logger = Logger ApacheLogger LogFlusher

initLogger :: IPAddrSource -> LogType -> IO Logger
initLogger ipsrc logtyp = do
    (aplgr, flusher) <- logInit ipsrc logtyp
    return $ Logger aplgr flusher

finLogger :: Logger -> LogFlusher
finLogger (Logger _ flusher) = flusher

apatcheLogger :: Logger -> ApacheLogger
apatcheLogger (Logger aplogr _) = aplogr
