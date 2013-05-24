{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Network.Wai.Application.Classic hiding ((</>), (+++))
import Network.Wai.Logger
import Network.Wai.Logger.Prefork
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix

import Config
import Daemon (background)
import Log
import Multi
import Net (listenSocket)
import Report
import Resource (amIrootUser, unlimit)
import Route
import Signal
import Single
import State
import Types

----------------------------------------------------------------

main :: IO ()
main = do
    (opt,route) <- getOptRoute
    let reportFile = opt_report_file opt
    rpt <- initReporter reportFile >>= checkReporter reportFile
    if opt_debug_mode opt then
        server opt route rpt
      else
        background opt $ server opt route rpt
  where
    getOptRoute = getArgs >>= eachCase
    eachCase args
      | n == 0 = do
          root <- amIrootUser
          let opt | root      = defaultOption { opt_port = 80 }
                  | otherwise = defaultOption
          dir <- getCurrentDirectory
          let dst = fromString . addTrailingPathSeparator $ dir
              route = [Block ["*"] [RouteFile "/" dst]]
          return (opt, route)
      | n == 2 = do
          let config_file = args !! 0
          routing_file <- getAbsoluteFile (args !! 1)
          opt   <- parseOption config_file
          route <- parseRoute  routing_file
          let opt' = opt {opt_routing_file = Just routing_file}
          return (opt',route)
      | otherwise = do
          hPutStrLn stderr "Usage: mighty"
          hPutStrLn stderr "       mighty config_file routing_file"
          exitFailure
      where
        n = length args
    getAbsoluteFile file
        | isAbsolute file = return file
        | otherwise       = do
            dir <- getCurrentDirectory
            return $ dir </> normalise file
    checkReporter _          (Right rpt) = return rpt
    checkReporter reportFile (Left e)    = do
        hPutStrLn stderr $ reportFile ++ " is not writable"
        hPrint stderr e
        exitFailure

----------------------------------------------------------------

server :: Option -> RouteDB -> Reporter -> IO ()
server opt route rpt = reportDo rpt $ do
    unlimit
    service <- openService opt
    unless debug writePidFile
    logCheck logtype
    myid <- getProcessID
    stt <- initStater
    if workers == 1 then do
        lgr <- initLogger FromSocket logtype
         -- killed by signal
        void . forkIO $ single opt route service rpt stt lgr
        void . forkIO $ logController logtype [myid]
        mainLoop rpt stt lgr
      else do
        cids <- multi opt route service logtype stt rpt
        void . forkIO $ logController logtype cids
        masterMainLoop rpt myid
  where
    debug = opt_debug_mode opt
    pidfile = opt_pid_file opt
    workers = opt_worker_processes opt
    writePidFile = do
        pid <- getProcessID
        writeFile pidfile $ show pid ++ "\n"
        setFileMode pidfile 0o644
    logspec = FileLogSpec {
        log_file          = opt_log_file opt
      , log_file_size     = fromIntegral $ opt_log_file_size opt
      , log_backup_number = opt_log_backup_number opt
      }
    logtype
      | not (opt_logging opt) = LogNone
      | debug                 = LogStdout
      | otherwise             = LogFile logspec sigLogCtl

openService :: Option -> IO Service
openService opt
  | service == 1 = do
      s <- listenSocket httpsPort
      debugMessage $ "HTTP/TLS service on port " ++ httpsPort ++ "."
      return $ HttpsOnly s
  | service == 2 = do
      s1 <- listenSocket httpPort
      s2 <- listenSocket httpsPort
      debugMessage $ "HTTP service on port " ++ httpPort ++ " and "
                  ++ "HTTP/TLS service on port " ++ httpsPort ++ "."
      return $ HttpAndHttps s1 s2
  | otherwise = do
      s <- listenSocket httpPort
      debugMessage $ "HTTP service on port " ++ httpPort ++ "."
      return $ HttpOnly s
  where
    httpPort  = show $ opt_port opt
    httpsPort = show $ opt_tls_port opt
    service = opt_service opt
    debug = opt_debug_mode opt
    debugMessage msg = when debug $ do
        putStrLn msg
        hFlush stdout
