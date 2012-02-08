{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.ByteString
import Data.ByteString.Char8 ()
import Data.Version
import Network.Wai.Application.Classic
import Paths_mighttpd2

type Src      = Path
type Dst      = Path
type Domain   = ByteString
type PathInfo = ByteString
type Port     = Int
data Block    = Block [Domain] [Route] deriving (Eq,Show)
data Route    = RouteFile     Src Dst
              | RouteCGI      Src Dst
              | RouteRevProxy Src Dst Domain Port
              deriving (Eq,Show)
type RouteDB  = [Block]

programName :: String
programName = "Mighttpd"

programVersion :: String
programVersion = showVersion version

defaultDomain :: Domain
defaultDomain = "localhost"

defaultPort :: Int
defaultPort = 80
