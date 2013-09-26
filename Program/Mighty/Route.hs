{-# LANGUAGE OverloadedStrings, TupleSections #-}

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
  ) where

import Control.Applicative hiding (many,(<|>))
import Control.Monad
import Data.ByteString
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Application.Classic
import Text.Parsec
import Text.Parsec.ByteString.Lazy

import Program.Mighty.Parser

----------------------------------------------------------------

-- | A logical path specified in URL.
type Src      = Path
-- | A physical path in a file system.
type Dst      = Path
type Domain   = ByteString
type Port     = Int
data Block    = Block [Domain] [Route] deriving (Eq,Show)
data Route    = RouteFile     Src Dst
              | RouteRedirect Src Dst
              | RouteCGI      Src Dst
              | RouteRevProxy Src Dst Domain Port
              deriving (Eq,Show)
type RouteDB  = [Block]

----------------------------------------------------------------

-- | Parsing a route file.
parseRoute :: FilePath
           -> Domain -- ^ A default domain, typically \"localhost\"
           -> Port   -- ^ A default port, typically 80.
           -> IO RouteDB
parseRoute file ddom dport = parseFile (routeDB ddom dport) file

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
    domain = BS.pack <$> many1 (noneOf "[], \t\n")
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
    fromByteString . BS.pack . (c:) <$> many (noneOf "[], \t\n") <* spcs

path' :: Parser Path
path' = fromByteString . BS.pack <$> many (noneOf "[], \t\n") <* spcs

-- [host1][:port2]/path2

domPortDst :: Domain -> Port -> Parser (Domain, Port, Dst)
domPortDst ddom dport = (ddom,,) <$> port <*> path
                    <|> try((,,) <$> domain <*> port <*> path)
                    <|> (,dport,) <$> domain <*> path
  where
    domain = BS.pack <$> many1 (noneOf ":/[], \t\n")
    port = do
        void $ char ':'
        read <$> many1 (oneOf ['0'..'9'])
