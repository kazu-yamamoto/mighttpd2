module Parser where

import Control.Applicative hiding (many)
import Text.Parsec
import Text.Parsec.ByteString.Lazy

spcs :: Parser ()
spcs = () <$ many spc

spcs1 :: Parser ()
spcs1 = () <$ many1 spc

spc :: Parser Char
spc = satisfy (\c -> c == ' ' || c == '\t')
