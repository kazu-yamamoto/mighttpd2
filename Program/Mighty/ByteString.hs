module Program.Mighty.ByteString where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- | Converting showalbe data to 'ByteString'.
bshow :: Show a => a -> ByteString
bshow = BS.pack . show

infixr 5 +++

-- | Appending two 'ByteString'.
(+++) :: ByteString -> ByteString -> ByteString
(+++) = BS.append
