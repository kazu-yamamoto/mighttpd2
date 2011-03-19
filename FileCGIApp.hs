{-# LANGUAGE OverloadedStrings #-}

module FileCGIApp (fileCgiApp) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.Wai
import Network.Wai.Application.Classic
import Types

fileCgiApp :: AppSpec -> URLMap -> Application
fileCgiApp spec um req = case mmp of
    Nothing -> return $ responseLBS statusNotFound
                                    [("Content-Type", "text/plain")]
                                    "Not found"
    Just (Mapper src op dst) -> case op of
        OpFile -> fileApp spec (FileRoute src dst) req
        OpCGI  -> cgiApp  spec (CgiRoute  src dst) req
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
  | src `BS.isPrefixOf` key = Just m
  | otherwise               = getMapper key ms
