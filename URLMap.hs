{-# LANGUAGE OverloadedStrings #-}

module URLMap (parseURLmap) where

import Control.Applicative hiding (many,(<|>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Parser
import System.IO
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Types

parseURLmap :: FilePath -> IO URLMap
parseURLmap file = do
    hdl <- openFile file ReadMode
    hSetEncoding hdl latin1
    bs <- BL.hGetContents hdl
    case parse urlmap "parseURLmap" bs of
        Right x -> return x
        Left  e -> error . show $ e

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
    open  = () <$ char '[' *> spcs
    close = () <$ char ']' *> spcs
    doms = (domain `sepBy1` sep) <* spcs
    domain = BS.pack <$> many1 (noneOf "[], \t\n")
    sep = () <$ spcs1

mapper :: Parser Mapper
mapper = Mapper <$> src <*> op <*> dst <* trailing
  where
    path = many1 (noneOf "[], \t\n")
    src = BS.pack <$> path <* spcs
    dst = path <* spcs
    op0 = OpFile <$ string "->" <|> OpCGI <$ string "=>"
    op  = op0 <* spcs

commentLines :: Parser ()
commentLines = () <$ many commentLine
  where
    commentLine = trailing

trailing :: Parser ()
trailing = () <$ (comment *> newline <|> newline)

comment :: Parser ()
comment = () <$ char '#' <* many (noneOf "\n")
