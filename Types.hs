module Types where

import Data.ByteString

type Src      = ByteString
type Dst      = ByteString
type Domain   = ByteString
type PathInfo = ByteString
data Block    = Block [Domain] [Route] deriving (Eq,Show)
data Route    = Route Src Op Dst deriving (Eq,Show)
data Op       = OpFile | OpCGI deriving (Eq,Show)
type RouteDB  = [Block]

programName :: String
programName = "Mighttpd"

programVersion :: String
programVersion = "2.3.1"
