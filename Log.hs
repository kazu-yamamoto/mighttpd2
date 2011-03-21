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
import System.IO
import System.Locale

fileInit :: FilePath -> IO (Chan ByteString)
fileInit path = do
    hdl <- open path
    mvar <- newMVar hdl
    chan <- newChan
    forkIO $ fileFlusher mvar path 128
    forkIO $ fileSerializer chan mvar
    return chan

locate :: FilePath -> IO ()
locate path = mapM_ move srcdsts
  where
    dsts = map (path++) [".3",".2",".1",".0",""]
    srcs = tail dsts
    srcdsts = zip srcs dsts
    move (src,dst) = do
        exist <- doesFileExist src
        when exist $ renameFile src dst

open :: FilePath -> IO Handle
open path = do
    hdl <- openFile path AppendMode
    hSetEncoding hdl latin1
    hSetBuffering hdl $ BlockBuffering (Just 16384)
    return hdl

fileFlusher :: MVar Handle -> FilePath -> Integer -> IO ()
fileFlusher mvar path lim = forever $ do
    threadDelay 10000000 -- 10 sec
    hdl <- takeMVar mvar
    hFlush hdl
    size <- hFileSize hdl
    if size > lim
       then do
        hClose hdl
        locate path
        newhdl <- open path
        putMVar mvar newhdl
       else putMVar mvar hdl

fileSerializer :: Chan ByteString -> MVar Handle -> IO ()
fileSerializer chan mvar = forever $ do
    xs <- readChan chan
    hdl <- takeMVar mvar
    BL.hPut hdl xs
    putMVar mvar hdl

----------------------------------------------------------------

setoutInit :: IO (Chan ByteString)
setoutInit = do
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
