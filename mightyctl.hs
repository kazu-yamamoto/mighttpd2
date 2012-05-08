module Main where

import System.Environment
import System.Posix.Signals
import Process

commandDB :: [(String, Signal)]
commandDB = [
    ("stop", softwareTermination)
--  ("reload", HUP)
--  ("quit", QUIT)
    ]

main :: IO ()
main = do
    [pid] <- getMightyPid
    [cmd] <- getArgs
    case lookup cmd commandDB of
        Just signal -> signalProcess signal pid
        Nothing -> putStrLn "No such command"
