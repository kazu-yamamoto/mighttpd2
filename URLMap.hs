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
urlmap = many block

block :: Parser Block
block = Block <$> domains <*> many mapper

domains :: Parser [Domain]
domains = (:) <$> domain <*> (many (sep *> domain)) <* garbage
  where
    domain = pack <$> many1 (noneOf ", \t\n")
    sep = () <$ spaces *> char ',' *> spaces
    garbage = () <$ spaces *> endOfLine

mapper :: Parser Mapper
mapper = Mapper <$> (spaces1 *> src) <*> op <*> dst <* endOfLine
  where
    path = many1 (noneOf ", \t\n") <* spaces
    src = pack <$> path
    dst = path
    op = OpFile <$ string "->" <* spaces
     <|> OpCGI  <$ string "=>" <* spaces
