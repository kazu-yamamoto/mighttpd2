module Parser where

import Control.Applicative hiding (many,(<|>))
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO
import Text.Parsec
import Text.Parsec.ByteString.Lazy

spcs :: Parser ()
spcs = () <$ many spc

spcs1 :: Parser ()
spcs1 = () <$ many1 spc

spc :: Parser Char
spc = satisfy (`elem` " \t")

commentLines :: Parser ()
commentLines = () <$ many commentLine
  where
    commentLine = trailing

trailing :: Parser ()
trailing = () <$ (comment *> newline <|> newline)

comment :: Parser ()
comment = () <$ char '#' <* many (noneOf "\n")

parseFile :: Parser a -> FilePath -> IO a
parseFile p file = do
    hdl <- openFile file ReadMode
    hSetEncoding hdl latin1
    bs <- BL.hGetContents hdl
    case parse p "parseFile" bs of
        Right x -> return x
        Left  e -> error . show $ e
