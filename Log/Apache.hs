{-# LANGUAGE OverloadedStrings #-}

module Log.Apache where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic
import Log.Types

apacheFormat :: ByteString -> Request -> Status -> Maybe Integer -> [LogStr]
apacheFormat tmstr req st msize = [
    LS addr
  , LB " - - ["
  , LB tmstr
  , LB "] \""
  , LB $ requestMethod req
  , LB " "
  , LB $ rawPathInfo req
  , LB "\" "
  , LS . show . statusCode $ st
  , LB " "
  , LS $ maybe "-" show msize
  , LB " \""
  , LB $ lookupRequestField' "referer" req
  , LB "\" \""
  , LB $ lookupRequestField' "user-agent" req
  , LB "\"\n"
  ]
  where
    addr = showSockAddr (remoteHost req)
