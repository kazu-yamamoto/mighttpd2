{-# LANGUAGE BangPatterns #-}

module Program.Mighty.LogMsg (
  -- * LogMsg
    LogMsg
  , fromByteString
  -- * LoggerSet
  , BufSize
  , LoggerSet
  , newLoggerSet
  , renewLoggerSet
  , pushLogMsg
  , flushLogMsg
  -- * Utilities
  , logOpen
  ) where

import qualified Blaze.ByteString.Builder as BD
import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Control.Concurrent
import Control.Monad
import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Monoid
import Data.Word (Word8)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.Posix.IO
import System.Posix.Types (Fd)

----------------------------------------------------------------

type Buffer = Ptr Word8
type BufSize = Int

----------------------------------------------------------------

-- | Log message builder. Use '(<>)' to append two LogMsg in O(1).
data LogMsg = LogMsg !Int Builder

instance Monoid LogMsg where
    mempty = LogMsg 0 (BD.fromByteString BS.empty)
    LogMsg s1 b1 `mappend` LogMsg s2 b2 = LogMsg (s1 + s2) (b1 <> b2)

-- | Creating 'LogMsg'
fromByteString :: ByteString -> LogMsg
fromByteString bs = LogMsg (BS.length bs) (BD.fromByteString bs)

----------------------------------------------------------------

-- | Writting 'LogMsg' using a buffer in blocking mode.
--   The size of 'LogMsg' must be smaller or equal to
--   the size of buffer.
writeLogMsg :: Fd
            -> Buffer
            -> BufSize
            -> LogMsg
            -> IO ()
writeLogMsg fd buf size (LogMsg len builder) = do
    if size < len then
        error "writeLogMsg"
      else
        toBufIOWith buf size (write fd) builder

toBufIOWith :: Buffer -> BufSize -> (Buffer -> Int -> IO a) -> Builder -> IO a
toBufIOWith buf !size io (Builder build) = do
    signal <- runBuildStep step bufRange
    case signal of
        Done ptr _ -> io buf (ptr `minusPtr` buf)
        _          -> error "toBufIOWith"
  where
    !step = build (buildStep finalStep)
    !bufRange = BufRange buf (buf `plusPtr` size)
    finalStep !(BufRange p _) = return $ Done p ()

write :: Fd -> Buffer -> Int -> IO ()
write fd buf len' = loop buf (fromIntegral len')
  where
    loop bf !len = do
        written <- fdWriteBuf fd bf len
        when (written < len) $ do
            loop (bf `plusPtr` (fromIntegral written)) (len - written)

----------------------------------------------------------------

data Logger = Logger Buffer !BufSize (IORef LogMsg)

newLogger :: BufSize -> IO Logger
newLogger size = do
    buf <- getBuffer size
    lref <- newIORef mempty
    return $ Logger buf size lref

pushLog :: Fd -> Logger -> LogMsg -> IO ()
pushLog fd logger@(Logger buf size lref) nlogmsg@(LogMsg nlen _) = do
    needFlush <- atomicModifyIORef lref check
    when needFlush $ do
        flushLog fd buf size lref
        -- Assuming that the buffer is never filled up during flushing
        pushLog fd logger nlogmsg
  where
    check ologmsg@(LogMsg olen _)
      | size < olen + nlen = (ologmsg, True)
      | otherwise          = (ologmsg <> nlogmsg, False)

flushLog :: Fd -> Buffer -> BufSize -> IORef LogMsg -> IO ()
flushLog fd buf size lref = do
    logmsg <- atomicModifyIORef lref (\old -> (mempty, old))
    writeLogMsg fd buf size logmsg

----------------------------------------------------------------

logOpen :: FilePath -> IO Fd
logOpen file = openFd file WriteOnly (Just 0o644) flags
  where
    flags = defaultFileFlags { append = True }

getBuffer :: BufSize -> IO Buffer
getBuffer = mallocBytes

----------------------------------------------------------------

data LoggerSet = LoggerSet (IORef Fd) (Array Int Logger) Buffer BufSize

newLoggerSet :: Fd -> BufSize -> IO LoggerSet
newLoggerSet fd size = do
    n <- getNumCapabilities
    loggers <- replicateM n $ newLogger size
    let arr = listArray (0,n-1) loggers
    fref <- newIORef fd
    buf <- getBuffer size
    return $ LoggerSet fref arr buf size

pushLogMsg :: LoggerSet -> LogMsg -> IO ()
pushLogMsg (LoggerSet fref arr _ _) logmsg = do
    (i, _) <- myThreadId >>= threadCapability
    let logger = arr ! i
    fd <- readIORef fref
    pushLog fd logger logmsg

-- 'buf' is critical section.
-- It is assumed that this code is called after enough interval
flushLogMsg :: LoggerSet -> IO ()
flushLogMsg (LoggerSet fref arr buf size) = do
    n <- getNumCapabilities
    fd <- readIORef fref
    mapM_ (flushIt fd) [0..n-1]
  where
    flushIt fd i = do
        let Logger _ _ lref = arr ! i
        flushLog fd buf size lref

-- | Renewing 'Fd' in 'LoggerSet'. Old 'Fd' is closed.
renewLoggerSet :: LoggerSet -> Fd -> IO ()
renewLoggerSet (LoggerSet fref _ _ _) newfd = do
    oldfd <- atomicModifyIORef fref (\fd -> (newfd, fd))
    closeFd oldfd
