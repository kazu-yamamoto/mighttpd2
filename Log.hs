{-# LANGUAGE OverloadedStrings #-}

module Log where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time
import Network.Wai
import Network.Wai.Application.Classic
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Locale
import System.Posix

data FileLogSpec = FileLogSpec {
    log_file :: String
  , log_file_size :: Integer
  , log_backup_number :: Int
  , log_buffer_size :: Int
  , log_flush_period :: Int
  }

fileCheck :: FileLogSpec -> IO ()
fileCheck spec = do
    dirperm <- getPermissions dir
    unless (writable dirperm) $ exit $ dir ++ " is not writable"
    fileexist <- doesFileExist file
    when fileexist $ do
        fileperm <- getPermissions file
        unless (writable fileperm) $ exit $ file ++ " is not writable"
  where
    file = log_file spec
    dir = takeDirectory file
    exit msg = hPutStrLn stderr msg >> exitFailure

fileInit :: FileLogSpec -> IO (Chan ByteString)
fileInit spec = do
    hdl <- open spec
    mvar <- newMVar hdl
    chan <- newChan
    forkIO $ fileFlusher mvar spec
    forkIO $ fileSerializer chan mvar
    let handler = fileFlushHandler mvar
    installHandler sigTERM handler Nothing
    installHandler sigKILL handler Nothing
    return chan

fileFlushHandler :: MVar Handle -> Handler
fileFlushHandler mvar = Catch $ do
    hdl <- takeMVar mvar
    hFlush hdl
    putMVar mvar hdl
    exitImmediately ExitSuccess

fileFlusher :: MVar Handle -> FileLogSpec -> IO ()
fileFlusher mvar spec = forever $ do
    threadDelay $ log_flush_period spec
    hdl <- takeMVar mvar
    hFlush hdl
    size <- hFileSize hdl
    if size > log_file_size spec
       then do
        hClose hdl
        locate spec
        newhdl <- open spec
        putMVar mvar newhdl
       else putMVar mvar hdl

fileSerializer :: Chan ByteString -> MVar Handle -> IO ()
fileSerializer chan mvar = forever $ do
    xs <- readChan chan
    hdl <- takeMVar mvar
    BL.hPut hdl xs
    putMVar mvar hdl

open :: FileLogSpec -> IO Handle
open spec = do
    hdl <- openFile file AppendMode
    setFileMode file 0o644
    hSetEncoding hdl latin1
    hSetBuffering hdl $ BlockBuffering (Just $ log_buffer_size spec)
    return hdl
  where
    file = log_file spec

locate :: FileLogSpec -> IO ()
locate spec = mapM_ move srcdsts
  where
    path = log_file spec
    n = log_backup_number spec
    dsts' = reverse . ("":) . map (('.':). show) $ [0..n-1]
    dsts = map (path++) dsts'
    srcs = tail dsts
    srcdsts = zip srcs dsts
    move (src,dst) = do
        exist <- doesFileExist src
        when exist $ renameFile src dst

----------------------------------------------------------------

stdoutInit :: IO (Chan ByteString)
stdoutInit = do
    chan <- newChan
    forkIO $ stdoutSerializer chan
    return chan

stdoutSerializer :: Chan ByteString -> IO ()
stdoutSerializer chan = forever $ readChan chan >>= BL.putStr

----------------------------------------------------------------

mightyLogger :: Chan ByteString -> Request -> Status -> IO ()
mightyLogger chan req st = do
    zt <- getZonedTime
    addr <- getPeerAddr (remoteHost req)
    writeChan chan $ BL.fromChunks (logmsg addr zt)
  where
    logmsg addr zt = [
        BS.pack addr
      , " - - ["
      , BS.pack (formatTime defaultTimeLocale "%d/%b/%Y:%T %z" zt)
      , "] \""
      , requestMethod req
      , " "
      , pathInfo req
      , "\" "
      , BS.pack (show . statusCode $ st)
      , " - \"" -- size
      , lookupRequestField' fkReferer req
      , "\" \""
      , lookupRequestField' fkUserAgent req
      , "\"\n"
      ]
