module Buffer where

import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.Posix.Types
import System.Posix.IO.ByteString

data Buffer = Buffer (Ptr Word8)
                     Int -- Buffer space size
                     Int -- Actually used length

createBuffer :: Int -> IO Buffer
createBuffer len = do
    ptr <- mallocBytes len
    return $ Buffer ptr len 0

copyByteString :: Buffer -> ByteString -> IO Buffer
copyByteString (Buffer dstp dsize dlen) (PS src soff slen) =
    withForeignPtr src $ \srcp' -> do
        let srcp = srcp' `plusPtr` soff
        memcpy dstp srcp (fromIntegral slen)
        return $ Buffer dstp dsize (dlen + slen)

copyByteStrings :: Buffer -> [ByteString] -> IO (Maybe Buffer)
copyByteStrings (Buffer dstp dsize dlen) ps'
  | slen > dsize - dlen = return Nothing
  | otherwise = go ps' (dstp `plusPtr` dlen)
  where
    slen = sum . map BS.length $ ps'
    go []            _   = return . Just $ Buffer dstp dsize (dlen + slen)
    go (PS p s l:ps) ptr = do
        withForeignPtr p $ \fp -> memcpy ptr (fp `plusPtr` s) (fromIntegral l)
        go ps (ptr `plusPtr` l)

clearBuffer :: Buffer -> Buffer
clearBuffer (Buffer ptr siz _) = Buffer ptr siz 0

writeBuffer :: Fd -> Buffer -> IO Buffer
writeBuffer fd (Buffer ptr siz len) = do
    fdWriteBuf fd ptr (fromIntegral len)
    return $ Buffer ptr siz 0
