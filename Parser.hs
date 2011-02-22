module Parser where

import Control.Applicative hiding (many)
import Data.Attoparsec.Char8

spaces :: Parser ()
spaces = () <$ many spc

spaces1 :: Parser ()
spaces1 = () <$ many1 spc

spc :: Parser Char
spc = satisfy (\c -> c == ' ' || c == '\t')

oneOf :: String -> Parser Char
oneOf = satisfy . inClass

noneOf :: String -> Parser Char
noneOf = satisfy . notInClass