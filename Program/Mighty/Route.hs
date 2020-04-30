{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# LANGUAGE CPP #-}

module Program.Mighty.Route (
  -- * Paring a routing file
    parseRoute
  -- * Types
  , RouteDB
  , Route(..)
  , Block(..)
  , Src
  , Dst
  , Domain
  , Port
  -- * RouteDBRef
  , RouteDBRef
  , newRouteDBRef
  , readRouteDBRef
  , writeRouteDBRef
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative hiding (many,(<|>))
#endif
import qualified Control.Applicative as A
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Natural (Natural)
import Network.Wai.Application.Classic
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Dhall (auto, input)

import Program.Mighty.Parser
import qualified Program.Mighty.Dhall.Route as DR
----------------------------------------------------------------

-- | A logical path specified in URL.
type Src      = Path
-- | A physical path in a file system.
type Dst      = Path
type Domain   = Text
type Port     = Natural

data Block    = Block [Domain] [Route] deriving (Eq,Show)
data Route    = RouteFile     Src Dst
              | RouteRedirect Src Dst
              | RouteCGI      Src Dst
              | RouteRevProxy Src Dst Domain Port
              deriving (Eq,Show)
type RouteDB  = [Block]

----------------------------------------------------------------
-- | Dhall conversions.

routeFromDhall :: DR.Route -> Route
routeFromDhall (DR.File dst src) = RouteFile (encodeUtf8 src) (encodeUtf8 dst)
routeFromDhall (DR.Redirect dst src) = RouteRedirect (encodeUtf8 src) (encodeUtf8 dst)
routeFromDhall (DR.Cgi dst src) = RouteCGI (encodeUtf8 src) (encodeUtf8 dst)
routeFromDhall (DR.RevProxy domain dst port src) = RouteRevProxy (encodeUtf8 src) (encodeUtf8 dst) domain port

blockFromDhall :: DR.Block -> Block
blockFromDhall (DR.MakeBlock doms routes) = Block doms (routeFromDhall <$> routes)

routeDbFromDhall :: DR.RouteDB -> RouteDB
routeDbFromDhall (DR.MakeRouteDB blocks) = fmap blockFromDhall blocks

----------------------------------------------------------------

-- | Parsing a route file.
parseRoute :: FilePath
           -> Domain -- ^ A default domain, typically \"localhost\"
           -> Port   -- ^ A default port, typically 80.
           -> IO RouteDB
parseRoute file ddom dport = parseRoutesTrad file ddom dport A.<|> parseRoutesDhall file

parseRoutesTrad :: FilePath
           -> Domain -- ^ A default domain, typically \"localhost\"
           -> Port   -- ^ A default port, typically 80.
           -> IO RouteDB
parseRoutesTrad file ddom dport = parseFile (routeDB ddom dport) file

parseRoutesDhall :: FilePath -> IO RouteDB
parseRoutesDhall = (fmap routeDbFromDhall) . parseRoutesDbDhall

parseRoutesDbDhall :: FilePath -> IO DR.RouteDB
parseRoutesDbDhall = input auto . fromString

routeDB :: Domain -> Port -> Parser RouteDB
routeDB ddom dport = commentLines *> many1 (block ddom dport) <* eof

block :: Domain -> Port -> Parser Block
block ddom dport = Block <$> cdomains <*> many croute
  where
    cdomains = domains <* commentLines
    croute   = route ddom dport  <* commentLines

domains :: Parser [Domain]
domains = open *> doms <* close <* trailing
  where
    open  = () <$ char '[' *> spcs
    close = () <$ char ']' *> spcs
    doms = (domain `sepBy1` sep) <* spcs
    domain = Data.Text.pack <$> many1 (noneOf "[], \t\n")
    sep = () <$ spcs1

data Op = OpFile | OpCGI | OpRevProxy | OpRedirect

route :: Domain -> Port -> Parser Route
route ddom dport = do
    s <- src
    o <- op
    case o of
        OpFile     -> RouteFile     s <$> dst <* trailing
        OpRedirect -> RouteRedirect s <$> dst' <* trailing
        OpCGI      -> RouteCGI      s <$> dst <* trailing
        OpRevProxy -> do
            (dom,prt,d) <- domPortDst ddom dport
            return $ RouteRevProxy s d dom prt
  where
    src = path
    dst = path
    dst' = path'
    op0 = OpFile     <$ string "->"
      <|> OpRedirect <$ string "<<"
      <|> OpCGI      <$ string "=>"
      <|> OpRevProxy <$ string ">>"
    op  = op0 <* spcs

path :: Parser Path
path = do
    c <- char '/'
    BS.pack . (c:) <$> many (noneOf "[], \t\n") <* spcs

path' :: Parser Path
path' = BS.pack <$> many (noneOf "[], \t\n") <* spcs

-- [host1][:port2]/path2

domPortDst :: Domain -> Port -> Parser (Domain, Port, Dst)
domPortDst ddom dport = (ddom,,) <$> port <*> path
                    <|> try((,,) <$> domain <*> port <*> path)
                    <|> (,dport,) <$> domain <*> path
  where
    domain = Data.Text.pack <$> many1 (noneOf ":/[], \t\n")
    port = do
        void $ char ':'
        read <$> many1 (oneOf ['0'..'9'])

----------------------------------------------------------------

newtype RouteDBRef = RouteDBRef (IORef RouteDB)

newRouteDBRef :: RouteDB -> IO RouteDBRef
newRouteDBRef rout = RouteDBRef <$> newIORef rout

readRouteDBRef :: RouteDBRef -> IO RouteDB
readRouteDBRef (RouteDBRef ref) = readIORef ref

writeRouteDBRef :: RouteDBRef -> RouteDB -> IO ()
writeRouteDBRef (RouteDBRef ref) rout = writeIORef ref rout
