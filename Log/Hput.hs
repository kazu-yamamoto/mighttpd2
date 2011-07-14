{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Log.Hput (hPutByteStrings) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..))
import Data.List
import Foreign
import GHC.Base
import GHC.IO.Buffer
import qualified GHC.IO.BufferedIO as Buffered
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Text
import GHC.IO.Handle.Types
import GHC.IORef
import GHC.Num
import GHC.Real

hPutByteStrings :: Handle -> [ByteString] -> IO ()
hPutByteStrings handle bss =
  wantWritableHandle "hPutByteStrings" handle $ \h_ -> bufsWrite h_ bss

bufsWrite :: Handle__-> [ByteString] -> IO ()
bufsWrite h_@Handle__{..} bss = do
    old_buf@Buffer{
        bufRaw = old_raw
      , bufR = w
      , bufSize = size
      } <- readIORef haByteBuffer
    if size - w > len then do
        withRawBuffer old_raw $ \ptr ->
            go (ptr `plusPtr` w)  bss
        writeIORef haByteBuffer old_buf{ bufR = w + len }
    else do
        old_buf' <- Buffered.flushWriteBuffer haDevice old_buf
        writeIORef haByteBuffer old_buf'
        bufsWrite h_ bss
  where
    len = foldl' (\x y -> x + BS.length y) 0 bss
    go :: Ptr Word8 -> [ByteString] -> IO ()
    go _ [] = return ()
    go dst (b:bs) = do
      dst' <- copy dst b
      go dst' bs

copy :: Ptr Word8 -> ByteString -> IO (Ptr Word8)
copy dst (PS ptr off len) = withForeignPtr ptr $ \s -> do
    let src = s `plusPtr` off
    memcpy dst src (fromIntegral len)
    return (dst `plusPtr` len)
