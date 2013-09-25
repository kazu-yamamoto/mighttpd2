module Program.Mighty.ByteString where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

bshow :: Show a => a -> ByteString
bshow = BS.pack . show

infixr 5 +++

(+++) :: ByteString -> ByteString -> ByteString
(+++) = BS.append

