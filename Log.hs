{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Log where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Time
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic
import System.Directory
import System.Locale
import System.Posix.IO hiding (fdWrite,fdWriteBuf)
import System.Posix.IO.ByteString
import System.Posix.Types

data FileLogSpec = FileLogSpec {
    log_file :: String
  , log_file_size :: Integer
  , log_backup_number :: Int
  }

newtype TimeRef = TimeRef (IORef ByteString)
newtype FdRef = FdRef (IORef Fd)

----------------------------------------------------------------

logInit :: FileLogSpec -> IO Logger
logInit spec = do
    timref <- clockInit
    fdref <- fileInit spec
    return $ apacheLogger timref fdref

----------------------------------------------------------------

getDate :: TimeRef -> IO ByteString
getDate (TimeRef ref) = readIORef ref

getFd :: FdRef -> IO Fd
getFd (FdRef ref) = readIORef ref

----------------------------------------------------------------

fileInit :: FileLogSpec -> IO FdRef
fileInit spec = open spec >>= (\ref -> FdRef <$> newIORef ref) 

open :: FileLogSpec -> IO Fd
open spec = openFd file WriteOnly (Just 0o644) defaultFileFlags { append = True }
  where
    file = log_file spec

rotate :: FileLogSpec -> IO ()
rotate spec = mapM_ move srcdsts
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

apacheLogger :: TimeRef -> FdRef -> Request -> Status -> Maybe Integer -> IO ()
apacheLogger timref fdref req st msize = do
    addr <- getPeerAddr (remoteHost req)
    tmstr <- getDate timref
    fd <- getFd fdref
    fdWrite fd $ BS.concat [
        BS.pack addr
      , " - - ["
      , tmstr
      , "] \""
      , requestMethod req
      , " "
      , rawPathInfo req
      , "\" "
      , BS.pack (show . statusCode $ st)
      , " "
      , BS.pack (maybe "-" show msize)
      , " \"" -- size
      , lookupRequestField' "referer" req
      , "\" \""
      , lookupRequestField' "user-agent" req
      , "\"\n"
      ]
    return ()

----------------------------------------------------------------

clockInit :: IO (TimeRef)
clockInit = do
    ref <- timeByteString >>= newIORef
    let timeref = TimeRef ref
    forkIO $ clock timeref
    return timeref

clock :: TimeRef -> IO ()
clock timeref@(TimeRef ref) = do
    tmstr <- timeByteString
    atomicModifyIORef ref (\_ -> (tmstr, undefined))
    threadDelay 1000000
    clock timeref

timeByteString :: IO ByteString
timeByteString =
    BS.pack . formatTime defaultTimeLocale "%d/%b/%Y:%T %z" <$> getZonedTime
