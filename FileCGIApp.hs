{-# LANGUAGE OverloadedStrings #-}

module FileCGIApp (fileCgiApp) where

import Data.ByteString.Char8 (ByteString, isPrefixOf)
import Network.Wai
import Network.Wai.Application.CGI
import Network.Wai.Application.File
import Types

fileCgiApp :: URLMap -> Application
fileCgiApp um req = case mmp of
    Nothing -> return $ responseLBS statusNotFound
                                    [("Content-Type", "text/plain")]
                                    "Not found"
    Just (Mapper src op dst) -> case op of
        OpFile -> fileApp (FileInfo src dst "index.html") req
        OpCGI  -> cgiApp  (CgiInfo  src dst "Mighttpd 2") req
  where
    mmp = getBlock (serverName req) um >>= getMapper (pathInfo req)

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
