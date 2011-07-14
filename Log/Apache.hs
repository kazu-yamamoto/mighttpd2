{-# LANGUAGE OverloadedStrings #-}

module Log.Apache where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic

apacheFormat :: ByteString -> Request -> Status -> Maybe Integer -> ByteString
apacheFormat tmstr req st msize = BS.concat [
    BS.pack addr
  , " - - ["
  , tmstr
  , "] \""
  , requestMethod req
  , " "
  , rawPathInfo req
  , "\" "
  , BS.pack (show . statusCode $ st)
  , " "
  , maybe "-" (BS.pack.show) msize
  , " \""
  , lookupRequestField' "referer" req
  , "\" \""
  , lookupRequestField' "user-agent" req
  , "\"\n"
  ]
  where
    addr = showSockAddr (remoteHost req)
