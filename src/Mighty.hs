{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

#ifndef HTTP_OVER_QUIC
import Control.Monad (when)
#endif

#ifdef DHALL
import Data.List (isSuffixOf)
#endif
import Data.Version (showVersion)
import Network.Wai.Application.Classic hiding ((</>))
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (addTrailingPathSeparator, isAbsolute, normalise, (</>))
import System.IO

import Program.Mighty

import Server
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
        debug = opt_debug_mode opt
        rpt = initReporter debug reportFile
        run = server opt rpt route
    if debug then run else background opt run
  where
    getOptRoute = getArgs >>= eachCase
    svrnm = programName ++ "/" ++ programVersion
    eachCase args
      | n == 0 = do
          root <- amIrootUser
          let opt0 = defaultOption { opt_server_name = svrnm }
          let opt | root      = opt0 { opt_port = 80 }
                  | otherwise = opt0
          dir <- getCurrentDirectory
          let dst = fromString . addTrailingPathSeparator $ dir
              route = [Block ["*"] [RouteFile "/" dst]]
          return (opt, route)
      | n == 2 = do
          let config_file = args !! 0
          routing_file <- getAbsoluteFile (args !! 1)
#ifdef DHALL
          let isDhall = ".dhall" `Data.List.isSuffixOf` config_file
          opt   <- if isDhall
                   then parseOptionDhall config_file
                   else parseOption config_file
#else
          opt   <- parseOption config_file
#endif
          route <- parseRoute  routing_file defaultDomain defaultPort
          let opt' = opt { opt_routing_file = Just routing_file
                         , opt_server_name = svrnm
                         }
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
#ifdef HTTP_OVER_TLS
#ifdef HTTP_OVER_QUIC
    checkTLS _ = return ()
#else
    checkTLS opt = when (opt_service opt > 2) $ do
        hPutStrLn stderr "This mighty does not support QUIC"
        exitFailure
#endif
#else
    checkTLS opt = when (opt_service opt > 1) $ do
        hPutStrLn stderr "This mighty does not support TLS"
        exitFailure
#endif

----------------------------------------------------------------

background :: Option -> IO () -> IO ()
background opt svr = do
    putStrLn $ "Serving on port " ++ show port ++ " and detaching this terminal..."
    putStrLn $ "(If errors occur, they will be written in \"" ++ reportFileName opt ++ "\".)"
    hFlush stdout
    daemonize svr
  where
    port = opt_port opt

reportFileName :: Option -> FilePath
reportFileName opt
  | port == 80 = rfile
  | otherwise  = rfile ++ show port
  where
    rfile = opt_report_file opt
    port = opt_port opt
