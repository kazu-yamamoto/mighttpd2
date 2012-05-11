{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Route (parseRoute) where

import Control.Applicative hiding (many,(<|>))
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Application.Classic
import Parser
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Types

parseRoute :: FilePath -> IO RouteDB
parseRoute = parseFile routeDB

routeDB :: Parser RouteDB
routeDB = commentLines *> many1 block <* eof

block :: Parser Block
block = Block <$> cdomains <*> many croute
  where
    cdomains = domains <* commentLines
    croute   = route   <* commentLines

domains :: Parser [Domain]
domains = open *> doms <* close <* trailing
  where
    open  = () <$ char '[' *> spcs
    close = () <$ char ']' *> spcs
    doms = (domain `sepBy1` sep) <* spcs
    domain = BS.pack <$> many1 (noneOf "[], \t\n")
    sep = () <$ spcs1

data Op = OpFile | OpCGI | OpRevProxy | OpRedirect

route :: Parser Route
route = do
    s <- src
    o <- op
    case o of
        OpFile     -> RouteFile     s <$> dst <* trailing
        OpRedirect -> RouteRedirect s <$> dst' <* trailing
        OpCGI      -> RouteCGI      s <$> dst <* trailing
        OpRevProxy -> do
            (dom,prt,d) <- domPortDst
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

domPortDst :: Parser (Domain, Port, Dst)
domPortDst = (defaultDomain,,) <$> port <*> path
         <|> try((,,) <$> domain <*> port <*> path)
         <|> (,defaultPort,) <$> domain <*> path
  where
    domain = BS.pack <$> many1 (noneOf ":/[], \t\n")
    port = do
        _ <- char ':'
        read <$> many1 (oneOf ['0'..'9'])
