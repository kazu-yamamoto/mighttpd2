{-# LANGUAGE DoAndIfThenElse #-}

module Log.File where

import Control.Applicative
import Control.Concurrent
import Control.Exception (handle, SomeException, catch)
import Control.Monad
import qualified Data.ByteString as BS
import Data.IORef
import Log.Apache
import Log.Date
import Log.Rotate
import Log.Types
import Prelude hiding (catch)
import System.IO
import System.Posix

----------------------------------------------------------------

logBufSize :: Int
logBufSize = 4096

----------------------------------------------------------------

newtype HandleRef = HandleRef (IORef Handle)

getHandle :: HandleRef -> IO Handle
getHandle (HandleRef ref) = readIORef ref

----------------------------------------------------------------

fileLoggerInit :: FileLogSpec -> IO Logger
fileLoggerInit spec = do
    hdl <- open spec
    hdlref <- HandleRef <$> newIORef hdl
    forkIO $ fileFlusher hdlref
    dateref <- dateInit
    installHandler sigUSR1 (Catch $ reopen spec hdlref) Nothing
    return $ fileLogger dateref hdlref

{-
 For BlockBuffering, hPut flushes the buffer before writing
 the target string. In other words, hPut does not split
 the target string. So, to implment multiple line buffering, 
 just use BlockBuffering.
-}
open :: FileLogSpec -> IO Handle
open spec = do
    hdl <- openFile file AppendMode
    hSetBuffering hdl (BlockBuffering (Just logBufSize))
    return hdl
  where
    file = log_file spec

reopen :: FileLogSpec -> HandleRef -> IO ()
reopen spec (HandleRef ref) = do
    hdl <- open spec
    oldhdl <- atomicModifyIORef ref (\oh -> (hdl,oh))
    hClose oldhdl

----------------------------------------------------------------

fileLogger :: DateRef -> HandleRef -> Logger
fileLogger dateref hdlref req status msiz = do
    date <- getDate dateref
    hdl <- getHandle hdlref
    BS.hPut hdl $ apacheFormat date req status msiz

fileFlusher :: HandleRef -> IO ()
fileFlusher hdlref = forever $ do
    threadDelay 10000000
    getHandle hdlref >>= hFlush

----------------------------------------------------------------

fileLoggerController :: FileLogSpec -> LogController
fileLoggerController spec pids = forever $ do
    isOver <- over
    when isOver $ do
        rotate spec
        mapM_ sendSignal pids
    threadDelay 10000000
  where
    file = log_file spec
    over = handle handler $ do
        siz <- fromIntegral . fileSize <$> getFileStatus file
        if siz > log_file_size spec then
            return True
        else
            return False
    sendSignal pid = signalProcess sigUSR1 pid `catch` ignore
    handler :: SomeException -> IO Bool
    handler _ = return False
    ignore :: SomeException -> IO ()
    ignore _ = return ()
