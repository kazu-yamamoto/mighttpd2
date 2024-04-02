{-# LANGUAGE CPP #-}

-- | Parsers for Mighty
module Program.Mighty.Parser (
    -- * Utilities
    parseFile,

    -- * Parsers
    spcs,
    spcs1,
    spc,
    commentLines,
    trailing,
    comment,
) where

import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import UnliftIO.Exception

-- $setup
-- >>> import Data.Either
-- >>> let isLeft = either (const True) (const False)

-- | Parsing a file.
--   If parsing fails, an 'IOException' is thrown.
parseFile :: Parser a -> FilePath -> IO a
parseFile p file = do
    hdl <- openFile file ReadMode
    hSetEncoding hdl latin1
    bs <- BL.hGetContents hdl
    case parse p "parseFile" bs of
        Right x -> return x
        Left e -> throwIO . userError . show $ e

-- | 'Parser' to consume zero or more white spaces
--
-- >>> parse spcs "" "    "
-- Right ()
-- >>> parse spcs "" ""
-- Right ()
spcs :: Parser ()
spcs = () <$ many spc

-- | 'Parser' to consume one or more white spaces
--
-- >>> parse spcs1 "" "    "
-- Right ()
-- >>> parse spcs1 "" " "
-- Right ()
-- >>> isLeft $ parse spcs1 "" ""
-- True
spcs1 :: Parser ()
spcs1 = () <$ many1 spc

-- | 'Parser' to consume exactly one white space
--
-- >>> parse spc "" " "
-- Right ' '
-- >>> isLeft $ parse spc "" ""
-- True
spc :: Parser Char
spc = satisfy (`elem` " \t")

-- | 'Parser' to consume one or more comment lines
--
-- >>> parse commentLines "" "# comments\n# comments\n# comments\n"
-- Right ()
commentLines :: Parser ()
commentLines = () <$ many commentLine
  where
    commentLine = trailing

-- | 'Parser' to consume a trailing comment
--
-- >>> parse trailing "" "# comments\n"
-- Right ()
-- >>> isLeft $ parse trailing "" "X# comments\n"
-- True
trailing :: Parser ()
trailing = () <$ (spcs *> comment *> newline <|> spcs *> newline)

-- | 'Parser' to consume a trailing comment
--
-- >>> parse comment "" "# comments"
-- Right ()
-- >>> isLeft $ parse comment "" "foo"
-- True
comment :: Parser ()
comment = () <$ char '#' <* many (noneOf "\n")
