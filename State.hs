{-# LANGUAGE BangPatterns #-}

module State where

import Data.IORef

data Status = Serving | Retiring deriving Eq

data State = State {
    connectionCounter :: !Int
  , serverStatus :: !Status
  }

initialState :: State
initialState = State 0 Serving

type StateRef = IORef State

initState :: IO (IORef State)
initState = newIORef initialState

getState :: IORef State -> IO State
getState = readIORef

retireStatus :: StateRef -> IO ()
retireStatus sref = do
    !_ <- atomicModifyIORef sref (\st -> (st {serverStatus = Retiring}, ()))
    return ()

increment :: IORef State -> IO ()
increment sref = do
    !_ <- atomicModifyIORef sref (\st -> (st {connectionCounter = connectionCounter st + 1}, ()))
    return ()

decrement :: IORef State -> IO ()
decrement sref = do
    !_ <- atomicModifyIORef sref (\st -> (st {connectionCounter = connectionCounter st - 1}, ()))
    return ()

