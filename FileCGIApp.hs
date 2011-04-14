{-# LANGUAGE OverloadedStrings #-}

module FileCGIApp (fileCgiApp) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic
import Types

fileCgiApp :: AppSpec -> RouteDB -> Application
fileCgiApp spec um req = case mmp of
    Nothing -> return $ responseLBS statusNotFound
                                    [("Content-Type", "text/plain")
                                    ,("Server", softwareName spec)]
                                    "Not Found\r\n"
    Just (Route src op dst) -> case op of
        OpFile -> fileApp spec (FileRoute src dst) req
        OpCGI  -> cgiApp  spec (CgiRoute  src dst) req
  where
    mmp = getBlock (serverName req) um >>= getRoute (rawPathInfo req)

getBlock :: ByteString -> RouteDB -> Maybe [Route]
getBlock _ [] = Nothing
getBlock key (Block doms maps : ms)
  | key `elem` doms = Just maps
  | otherwise       = getBlock key ms

getRoute :: ByteString -> [Route] -> Maybe Route
getRoute _ [] = Nothing
getRoute key (m@(Route src _ _):ms)
  | src `BS.isPrefixOf` key = Just m
  | otherwise               = getRoute key ms
