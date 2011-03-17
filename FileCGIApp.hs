{-# LANGUAGE OverloadedStrings #-}

module FileCGIApp (fileCgiApp) where

import Config
import Data.ByteString.Char8 (ByteString, isPrefixOf, pack)
import Data.List (isSuffixOf)
import Network.Wai
import Network.Wai.Application.CGI
import Network.Wai.Application.File
import Types

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
        softwareName = pack $ opt_server_name opt
      , indexFile = opt_index_file opt
      , isHTML = \x -> ".html" `isSuffixOf` x || ".htm" `isSuffixOf` x
      }

getBlock :: ByteString -> URLMap -> Maybe [Mapper]
getBlock _ [] = Nothing
getBlock key (Block doms maps : ms)
  | key `elem` doms = Just maps
  | otherwise       = getBlock key ms

getMapper :: ByteString -> [Mapper] -> Maybe Mapper
getMapper _ [] = Nothing
getMapper key (m@(Mapper src _ _):ms)
  | src `isPrefixOf` key = Just m
  | otherwise            = getMapper key ms
