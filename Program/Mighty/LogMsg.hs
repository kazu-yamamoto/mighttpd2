{-# LANGUAGE BangPatterns #-}

module Program.Mighty.LogMsg (
    LogMsg
  , fromByteString
  , writeLogMsg
  ) where

import qualified Blaze.ByteString.Builder as BD
import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import Data.ByteString.Char8 ()
import Data.Monoid
import Data.Word (Word8)
import Foreign.Ptr
import System.Posix.IO
import System.Posix.Types (Fd)

----------------------------------------------------------------

-- | Log message builder. Use '(<>)' to append two LogMsg in O(1).
data LogMsg = LogMsg !Int Builder

instance Monoid LogMsg where
    mempty = LogMsg 0 (BD.fromByteString BS.empty)
    LogMsg s1 b1 `mappend` LogMsg s2 b2 = LogMsg (s1 + s2) (b1 <> b2)

-- | Creating 'LogMsg'
fromByteString :: ByteString -> LogMsg
fromByteString bs = LogMsg (BS.length bs) (BD.fromByteString bs)

-- | Writting 'LogMsg' using a buffer in blocking mode.
--   The size of 'LogMsg' must be smaller or equal to
--   the size of buffer.
writeLogMsg :: Fd
            -> Ptr Word8 -- ^ buffer
            -> Int       -- ^ buffer size
            -> LogMsg
            -> IO ()
writeLogMsg fd buf size (LogMsg len builder) = do
    if size < len then
        error "writeLogMsg"
      else
        toBufIOWith buf size (write fd) builder

toBufIOWith :: Ptr Word8 -> Int -> (Ptr Word8 -> Int -> IO a) -> Builder -> IO a
toBufIOWith buf !size io (Builder build) = do
    signal <- runBuildStep step range
    case signal of
        Done ptr _ -> io buf (ptr `minusPtr` buf)
        _          -> error "toBufIOWith"
  where
    !step = build (buildStep finalStep)
    !range = BufRange buf (buf `plusPtr` size)
    finalStep !(BufRange p _) = return $ Done p ()

write :: Fd -> Ptr Word8 -> Int -> IO ()
write fd buf size = loop buf (fromIntegral size)
  where
    loop bf !sz = do
        written <- fdWriteBuf fd bf sz
        when (written < sz) $ do
            loop (bf `plusPtr` (fromIntegral written)) (sz - written)
