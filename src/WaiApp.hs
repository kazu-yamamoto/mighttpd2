{-# LANGUAGE OverloadedStrings, CPP #-}

module WaiApp (fileCgiApp) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (isPrefixOf, length)
import Network.HTTP.Types (preconditionFailed412, movedPermanently301, urlDecode)
import Network.Wai (Application, responseLBS)
import Network.Wai.Internal
import Network.Wai.Application.Classic

import Program.Mighty

data Perhaps a = Found a | Redirect | Fail

fileCgiApp :: ClassicAppSpec -> FileAppSpec -> CgiAppSpec -> RevProxyAppSpec
           -> RouteDBRef -> Application
fileCgiApp cspec filespec cgispec revproxyspec rdr req respond = do
    um <- readRouteDBRef rdr
    case mmp um of
        Fail -> do
            let st = preconditionFailed412
            liftIO $ logger cspec req' st Nothing
            fastResponse respond st defaultHeader "Precondition Failed\r\n"
        Redirect -> do
            let st = movedPermanently301
                hdr = defaultHeader ++ redirectHeader req'
            liftIO $ logger cspec req st Nothing
            fastResponse respond st hdr "Moved Permanently\r\n"
        Found (RouteFile  src dst) ->
            fileApp cspec filespec (FileRoute src dst) req' respond
        Found (RouteRedirect src dst) ->
            redirectApp cspec (RedirectRoute src dst) req' respond
        Found (RouteCGI   src dst) ->
            cgiApp cspec cgispec (CgiRoute src dst) req' respond
        Found (RouteRevProxy src dst dom prt) ->
            revProxyApp cspec revproxyspec (RevProxyRoute src dst dom prt) req respond
  where
    (host, _) = hostPort req
    path = urlDecode False $ rawPathInfo req
    mmp um = case getBlock host um of
        Nothing  -> Fail
        Just blk -> getRoute path blk
    fastResponse resp st hdr body = resp $ responseLBS st hdr body
    defaultHeader = [("Content-Type", "text/plain")
                    ,("Server", softwareName cspec)]
    req' = req { rawPathInfo = path } -- FIXME

getBlock :: ByteString -> RouteDB -> Maybe [Route]
getBlock _ [] = Nothing
getBlock key (Block doms maps : ms)
  | "*" `elem` doms = Just maps
  | key `elem` doms = Just maps
  | otherwise       = getBlock key ms

getRoute :: ByteString -> [Route] -> Perhaps Route
getRoute _ []                = Fail
getRoute key (m:ms)
  | src `isPrefixOf` key     = Found m
  | src `isMountPointOf` key = Redirect
  | otherwise                = getRoute key ms
  where
    src = routeSource m

routeSource :: Route -> Src
routeSource (RouteFile     src _)     = src
routeSource (RouteRedirect src _)     = src
routeSource (RouteCGI      src _)     = src
routeSource (RouteRevProxy src _ _ _) = src

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
