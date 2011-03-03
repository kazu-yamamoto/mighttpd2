module Types where

import Data.ByteString

type Src      = ByteString
type Dst      = FilePath
type Domain   = ByteString
type PathInfo = ByteString
data Block    = Block [Domain] [Mapper] deriving Show
data Mapper   = Mapper Src Op Dst deriving Show
data Op       = OpFile | OpCGI deriving Show
type URLMap = [Block]

programName :: String
programName = "Mighttpd"

programVersion :: String
programVersion = "2.0.0"
