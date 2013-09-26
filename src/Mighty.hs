{-# LANGUAGE OverloadedStrings, CPP #-}

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
import Data.Version

import Program.Mighty

import Config
import Log
import Route
import Single
import Types
import Paths_mighttpd2 as P

----------------------------------------------------------------

programName :: String
programName = "Mighttpd"

programVersion :: String
programVersion = showVersion P.version

----------------------------------------------------------------

main :: IO ()
main = do
    (opt,route) <- getOptRoute
    checkTLS opt
    let reportFile = reportFileName opt
    rpt <- initReporter reportFile >>= checkReporter reportFile
    if opt_debug_mode opt then
        server opt route rpt
      else
        background opt $ server opt route rpt
  where
    getOptRoute = getArgs >>= eachCase
    svrnm = programName ++ "/" ++ programVersion
    eachCase args
      | n == 0 = do
          root <- amIrootUser
          let opt | root      = (defaultOption svrnm) { opt_port = 80 }
                  | otherwise = defaultOption svrnm
          dir <- getCurrentDirectory
          let dst = fromString . addTrailingPathSeparator $ dir
              route = [Block ["*"] [RouteFile "/" dst]]
          return (opt, route)
      | n == 2 = do
          let config_file = args !! 0
          routing_file <- getAbsoluteFile (args !! 1)
          opt   <- parseOption config_file svrnm
          route <- parseRoute  routing_file defaultDomain defaultPort
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
    reportFileName opt
      | port == 80 = rfile
      | otherwise  = rfile ++ show port
      where
        rfile = opt_report_file opt
        port = opt_port opt
    checkReporter _          (Right rpt) = return rpt
    checkReporter reportFile (Left e)    = do
        hPutStrLn stderr $ reportFile ++ " is not writable"
        hPrint stderr e
        exitFailure
#ifdef TLS
    checkTLS _ = return ()
#else
    checkTLS opt = when (opt_service opt > 1) $ do
        hPutStrLn stderr "This mighty does not support TLS"
        exitFailure
#endif

----------------------------------------------------------------

server :: Option -> RouteDB -> Reporter -> IO ()
server opt route rpt = reportDo rpt $ do
    unlimit
    service <- openService opt
    unless debug writePidFile
    logCheck logtype
    myid <- getProcessID
    stt <- initStater
    lgr <- initLogger FromSocket logtype
    -- killed by signal
    void . forkIO $ single opt route service rpt stt lgr
    void . forkIO $ logController logtype [myid]
    mainLoop rpt stt lgr
  where
    debug = opt_debug_mode opt
    pidfile = opt_pid_file opt
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

background :: Option -> IO () -> IO ()
background opt svr = do
    putStrLn $ "Serving on port " ++ show port ++ " and detaching this terminal..."
    putStrLn $ "(If errors occur, they will be written in \"" ++ opt_report_file opt ++ "\".)"
    hFlush stdout
    daemonize svr
  where
    port = opt_port opt
