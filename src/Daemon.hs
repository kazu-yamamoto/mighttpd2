module Daemon (background) where

import Program.Mighty
import Config
import System.IO

background :: Option -> IO () -> IO ()
background opt svr = do
    putStrLn $ "Serving on port " ++ show port ++ " and detaching this terminal..."
    putStrLn $ "(If errors occur, they will be written in \"" ++ opt_report_file opt ++ "\".)"
    hFlush stdout
    daemonize svr
  where
    port = opt_port opt
