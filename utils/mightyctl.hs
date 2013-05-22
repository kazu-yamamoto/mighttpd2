module Main where

import Data.List
import Process (getMightyPid)
import Signal
import System.Environment
import System.Exit
import System.Posix.Signals
import System.Posix.Types

commandDB :: [(String, Signal)]
commandDB = [
    ("stop",   sigStop)
  , ("reload", sigReload)
  , ("retire", sigRetire)
  , ("info",   sigInfo)
  ]

usage :: IO a
usage = do
    putStrLn "Usage:"
    putStrLn $ "    mightyctl " ++ cmds ++ " [pid]"
    exitFailure
  where
    cmds = intercalate "|" $ map fst commandDB

main :: IO ()
main = do
    (sig,mpid) <- getArgs >>= checkArgs
    pid <- maybe getProcessIdWithPS return mpid
    signalProcess sig pid

checkArgs :: [String] -> IO (Signal, Maybe ProcessID)
checkArgs [cmd]     = do
    sig <- getSignal cmd
    return (sig, Nothing)
checkArgs [cmd,num] = do
    sig <- getSignal cmd
    pid <- getProcessId num
    return (sig, Just pid)
checkArgs _         = usage

getSignal :: String -> IO Signal
getSignal cmd = check $ lookup cmd commandDB
  where
    check (Just sig) = return sig
    check Nothing    = do
        putStrLn $ "No such command: " ++ cmd
        usage

getProcessId :: String -> IO ProcessID
getProcessId num = check $ reads num
  where
    check [(pid,"")] = return . fromIntegral $ (pid :: Int)
    check _          = do
        putStrLn $ "No such process id: " ++ num
        usage

getProcessIdWithPS :: IO ProcessID
getProcessIdWithPS = getMightyPid >>= check
  where
    check []    = putStrLn "No Mighty found" >> usage
    check [pid] = return pid
    check pids  = do
        putStrLn $ "Multiple Mighty found: " ++ intercalate ", " (map show pids)
        usage
