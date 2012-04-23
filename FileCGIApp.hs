{-# LANGUAGE OverloadedStrings #-}

module FileCGIApp (fileCgiApp) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic
import Types

data Perhaps a = Found a | Redirect | Fail

fileCgiApp :: ClassicAppSpec -> FileAppSpec -> CgiAppSpec -> RevProxyAppSpec -> RouteDB -> Application
fileCgiApp cspec filespec cgispec revproxyspec um req = case mmp of
    Fail -> do
        let st = preconditionFailed412
        liftIO $ logger cspec req st Nothing
        fastResponse st defaultHeader "Precondition Failed\r\n"
    Redirect -> do
        let st = movedPermanently301
            hdr = defaultHeader ++ redirectHeader req
        liftIO $ logger cspec req st Nothing
        fastResponse st hdr "Moved Permanently\r\n"
    Found (RouteFile  src dst) -> do
        fileApp cspec filespec (FileRoute src dst) req
    Found (RouteCGI   src dst) ->
        cgiApp cspec cgispec (CgiRoute src dst) req
    Found (RouteRevProxy src dst dom prt) ->
        revProxyApp cspec revproxyspec (RevProxyRoute src dst dom prt) req
  where
    mmp = case getBlock (serverName req) um of
        Nothing  -> Fail
        Just blk -> getRoute (rawPathInfo req) blk
    fastResponse st hdr body = return $ responseLBS st hdr body
    defaultHeader = [("Content-Type", "text/plain")
                    ,("Server", softwareName cspec)]

getBlock :: ByteString -> RouteDB -> Maybe [Route]
getBlock _ [] = Nothing
getBlock key (Block doms maps : ms)
  | "*" `elem` doms = Just maps
  | key `elem` doms = Just maps
  | otherwise       = getBlock key ms

getRoute :: ByteString -> [Route] -> Perhaps Route
getRoute _ []                = Fail
getRoute key (m@(RouteFile src _):ms)
  | src `isPrefixOf` key     = Found m
  | src `isMountPointOf` key = Redirect
  | otherwise                = getRoute key ms
getRoute key (m@(RouteCGI src _):ms)
  | src `isPrefixOf` key     = Found m
  | src `isMountPointOf` key = Redirect
  | otherwise                = getRoute key ms
getRoute key (m@(RouteRevProxy src _ _ _):ms)
  | src `isPrefixOf` key     = Found m
  | src `isMountPointOf` key = Redirect
  | otherwise                = getRoute key ms

isPrefixOf :: Path -> ByteString -> Bool
isPrefixOf src key = src' `BS.isPrefixOf` key
  where
    src' = pathByteString src

isMountPointOf :: Path -> ByteString -> Bool
isMountPointOf src key = hasTrailingPathSeparator src
                      && BS.length src' - BS.length key == 1
                      && key `BS.isPrefixOf` src'
  where
    src' = pathByteString src
