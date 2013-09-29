{-# LANGUAGE OverloadedStrings #-}

module Program.Mighty.Apache (
    IPAddrSource(..)
  , apacheLogMsg
  ) where

import Program.Mighty.LogMsg
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive
import Data.List
import Data.Maybe
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Logger.Utils
import System.Log.FastLogger

-- | Source from which the IP source address of the client is obtained.
data IPAddrSource =
  -- | From the peer address of the HTTP connection.
    FromSocket
  -- | From X-Real-IP: or X-Forwarded-For: in the HTTP header.
  | FromHeader

apacheLogMsg :: IPAddrSource -> ZonedDate -> Request -> Status -> Maybe Integer -> LogMsg
apacheLogMsg ipsrc tmstr req status msize =
      getSourceIP ipsrc req
  +++ bs " - - ["
  +++ bs tmstr
  +++ bs "] \""
  +++ bs (requestMethod req)
  +++ bs " "
  +++ bs (rawPathInfo req)
  +++ bs " "
  +++ st (show (httpVersion req))
  +++ bs "\" "
  +++ st (show (statusCode status))
  +++ bs " "
  +++ st (maybe "-" show msize)
  +++ bs " \""
  +++ bs (lookupRequestField' "referer" req)
  +++ bs "\" \""
  +++ bs (lookupRequestField' "user-agent" req)
  +++ bs "\"\n"
  where
    st = bs . BS.pack
    bs = fromByteString
    (+++) = mappend

lookupRequestField' :: CI ByteString -> Request -> ByteString
lookupRequestField' k req = fromMaybe "" . lookup k $ requestHeaders req

getSourceIP :: IPAddrSource -> Request -> LogMsg
getSourceIP FromSocket = fromByteString . BS.pack . showSockAddr . remoteHost
getSourceIP FromHeader = fromByteString . getSource

getSource :: Request -> ByteString
getSource req = addr
  where
    maddr = find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
    addr = maybe "" snd maddr
    hdrs = requestHeaders req
