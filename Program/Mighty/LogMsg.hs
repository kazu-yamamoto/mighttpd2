{-# LANGUAGE BangPatterns #-}

module Program.Mighty.LogMsg (
  -- * LogMsg
    LogMsg
  , fromByteString
  -- * Logger
  , Buffer
  , BufSize
  , Logger(..)
  , newLogger
  , renewLogger
  -- * Logging
  , pushLogMsg
  , flushLogMsg
  -- * Utilities
  , logOpen
  -- * LoggerSet
  , LoggerSet
  , newLoggerSet
  , pushLogMsg'
  , flushLogMsg'
  ) where

import qualified Blaze.ByteString.Builder as BD
import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Control.Concurrent
import Control.Monad
import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import Data.ByteString.Char8 ()
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

data Logger = Logger (IORef Fd) (MVar Buffer) !BufSize (IORef LogMsg)

newLogger :: Fd -> BufSize -> IO Logger
newLogger fd size = do
    fref <- newIORef fd
    buf <- getBuffer size
    mbuf <- newMVar buf
    lref <- newIORef mempty
    return $ Logger fref mbuf size lref

renewLogger :: Logger -> Fd -> IO ()
renewLogger (Logger fref _ _ _) newfd = do
    oldfd <- atomicModifyIORef fref (\fd -> (newfd, fd))
    closeFd oldfd

pushLogMsg :: Logger -> LogMsg -> IO ()
pushLogMsg logger@(Logger _ _ size ref) nlogmsg@(LogMsg nlen _) = do
    needFlush <- atomicModifyIORef ref check
    when needFlush $ do
        flushLogMsg logger
        pushLogMsg logger nlogmsg
  where
    check ologmsg@(LogMsg olen _)
      | size < olen + nlen = (ologmsg, True)
      | otherwise          = (ologmsg <> nlogmsg, False)

flushLogMsg :: Logger -> IO ()
flushLogMsg (Logger fref mbuf size lref) = do
    logmsg <- atomicModifyIORef lref (\old -> (mempty, old))
    buf <- takeMVar mbuf
    fd <- readIORef fref
    writeLogMsg fd buf size logmsg
    putMVar mbuf buf

----------------------------------------------------------------

logOpen :: FilePath -> IO Fd
logOpen file = openFd file WriteOnly (Just 0o644) flags
  where
    flags = defaultFileFlags { append = True }

getBuffer :: BufSize -> IO Buffer
getBuffer = mallocBytes

----------------------------------------------------------------

type LoggerSet = Array Int Logger

newLoggerSet :: Fd -> BufSize -> IO (Array Int Logger)
newLoggerSet fd size = do
    n <- getNumCapabilities
    loggers <- replicateM n $ newLogger fd size
    return $ listArray (0,n-1) loggers

getLogger :: LoggerSet -> IO Logger
getLogger loggerset = do
    (i, _) <- myThreadId >>= threadCapability
    return $! loggerset ! i

pushLogMsg' :: LoggerSet -> LogMsg -> IO ()
pushLogMsg' loggerset logmsg = do
    logger <- getLogger loggerset
    pushLogMsg logger logmsg

flushLogMsg' :: LoggerSet -> IO ()
flushLogMsg' loggerset = do
    n <- getNumCapabilities
    mapM_ flushIt [0..n-1]
  where
    flushIt i = flushLogMsg (loggerset ! i)
