{-# LANGUAGE OverloadedStrings #-}

module FileCGIApp (fileCgiApp) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic
import Types

fileCgiApp :: ClassicAppSpec -> FileAppSpec -> RevProxyAppSpec -> RouteDB -> Application
fileCgiApp cspec filespec revproxyspec um req = case mmp of
    Nothing -> return $ responseLBS statusNotFound
                                    [("Content-Type", "text/plain")
                                    ,("Server", softwareName cspec)]
                                    "Not Found\r\n"
    Just (RouteFile  src dst) -> fileApp cspec filespec (FileRoute src dst) req
    Just (RouteCGI   src dst) -> cgiApp cspec (CgiRoute  src dst) req
    Just (RouteRevProxy src dst dom prt) ->
        revProxyApp cspec revproxyspec (RevProxyRoute src dst dom prt) req
  where
    mmp = getBlock (serverName req) um >>= getRoute (rawPathInfo req)

getBlock :: ByteString -> RouteDB -> Maybe [Route]
getBlock _ [] = Nothing
getBlock key (Block doms maps : ms)
  | key `elem` doms = Just maps
  | otherwise       = getBlock key ms

getRoute :: ByteString -> [Route] -> Maybe Route
getRoute _ [] = Nothing
getRoute key (m@(RouteFile src _):ms)
  | src `BS.isPrefixOf` key = Just m
  | otherwise               = getRoute key ms
getRoute key (m@(RouteCGI src _):ms)
  | src `BS.isPrefixOf` key = Just m
  | otherwise               = getRoute key ms
getRoute key (m@(RouteRevProxy src _ _ _):ms)
  | src `BS.isPrefixOf` key = Just m
  | otherwise               = getRoute key ms
