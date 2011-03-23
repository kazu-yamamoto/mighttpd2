{-# LANGUAGE OverloadedStrings #-}

module Route (parseRoute) where

import Control.Applicative hiding (many,(<|>))
import qualified Data.ByteString.Char8 as BS
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

route :: Parser Route
route = Route <$> src <*> op <*> dst <* trailing
  where
    path = many1 (noneOf "[], \t\n")
    src = BS.pack <$> path <* spcs
    dst = path <* spcs
    op0 = OpFile <$ string "->" <|> OpCGI <$ string "=>"
    op  = op0 <* spcs
