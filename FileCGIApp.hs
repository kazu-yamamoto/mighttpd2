{-# LANGUAGE OverloadedStrings #-}

module FileCGIApp (fileCgiApp) where

import Config
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (isSuffixOf)
import Network.Wai
import Network.Wai.Application.CGI
import Network.Wai.Application.File
import Types
import Data.Time
import System.Locale

fileCgiApp :: Option -> URLMap -> Application
fileCgiApp opt um req = case mmp of
    Nothing -> return $ responseLBS statusNotFound
                                    [("Content-Type", "text/plain")]
                                    "Not found"
    Just (Mapper src op dst) -> case op of
        OpFile -> fileApp spec (FileRoute src dst) req
        OpCGI  -> cgiApp  spec (CgiRoute  src dst) req
  where
    mmp = getBlock (serverName req) um >>= getMapper (pathInfo req)
    spec = AppSpec {
        softwareName = BS.pack $ opt_server_name opt
      , indexFile = opt_index_file opt
      , isHTML = \x -> ".html" `isSuffixOf` x || ".htm" `isSuffixOf` x
      , logger = mightyLogger
      }

getBlock :: ByteString -> URLMap -> Maybe [Mapper]
getBlock _ [] = Nothing
getBlock key (Block doms maps : ms)
  | key `elem` doms = Just maps
  | otherwise       = getBlock key ms

getMapper :: ByteString -> [Mapper] -> Maybe Mapper
getMapper _ [] = Nothing
getMapper key (m@(Mapper src _ _):ms)
  | src `BS.isPrefixOf` key = Just m
  | otherwise               = getMapper key ms

mightyLogger :: Request -> RspSpec -> IO ()
mightyLogger req rsp = do
    (getPeerAddr $ remoteHost req) >>= putStr
    BS.putStr " - - ["
    zt <- getZonedTime
    putStr $ formatTime defaultTimeLocale "%d/%b/%Y:%T %z" zt
    BS.putStr "] \""
    BS.putStr $ requestMethod req
    BS.putStr " "
    BS.putStr $ pathInfo req
    BS.putStr "\" "
    putStr . show . statusCode . rspStatus $ rsp
    BS.putStr " - \"" -- size
    BS.putStr $ lookupRequestField' fkReferer req
    BS.putStr "\" \""
    BS.putStr $ lookupRequestField' fkUserAgent req
    BS.putStr "\"\n"
