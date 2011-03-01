{-# LANGUAGE OverloadedStrings #-}

module URLMap (parseURLmap) where

import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.Attoparsec.Enumerator
import Data.ByteString.Char8 hiding (map)
import Data.Enumerator (Iteratee, run_, ($$))
import Data.Enumerator.Binary (enumFile)
import Types
import Parser

parseURLmap :: FilePath -> IO URLMap
parseURLmap file = run_ $ enumFile file $$ iterURLmap

iterURLmap :: Iteratee ByteString IO URLMap
iterURLmap = iterParser urlmap

urlmap :: Parser URLMap
urlmap = commentLines *> many block

block :: Parser Block
block = Block <$> cdomains <*> many cmapper
  where
    cdomains = domains <* commentLines
    cmapper  = mapper  <* commentLines

domains :: Parser [Domain]
domains = open *> doms <* close <* trailing
  where
    open  = () <$ char '[' *> spaces
    close = () <$ char ']' *> spaces
    doms = (domain `sepBy1` sep) <* spaces
    domain = pack <$> many1 (noneOf "[], \t\n")
    sep = () <$ spaces1

mapper :: Parser Mapper
mapper = Mapper <$> src <*> op <*> dst <* trailing
  where
    path = many1 (noneOf "[], \t\n")
    src = pack <$> path <* spaces
    dst = path <* spaces
    op0 = OpFile <$ string "->" <|> OpCGI <$ string "=>"
    op  = op0 <* spaces

commentLines :: Parser ()
commentLines = () <$ many commentLine
  where
    commentLine = trailing

trailing :: Parser ()
trailing = comment *> endOfLine <|> endOfLine

comment :: Parser ()
comment = () <$ char '#' <* many (notChar '\n')
