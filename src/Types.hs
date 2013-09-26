{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.ByteString
import Data.ByteString.Char8 ()
import Network (Socket)
import Network.Wai.Application.Classic

type Src      = Path
type Dst      = Path
type Domain   = ByteString
type PathInfo = ByteString
type Port     = Int
data Block    = Block [Domain] [Route] deriving (Eq,Show)
data Route    = RouteFile     Src Dst
              | RouteRedirect Src Dst
              | RouteCGI      Src Dst
              | RouteRevProxy Src Dst Domain Port
              deriving (Eq,Show)
type RouteDB  = [Block]

defaultDomain :: Domain
defaultDomain = "localhost"

defaultPort :: Int
defaultPort = 80

data Service = HttpOnly Socket | HttpsOnly Socket | HttpAndHttps Socket Socket
