{-# LANGUAGE BangPatterns #-}

module State where

import Control.Concurrent
import Data.IORef

data Status = Serving | Retiring deriving Eq

data State = State {
    connectionCounter :: !Int
  , serverStatus :: !Status
  , warpThreadId :: !(Maybe ThreadId)
  }

initialState :: State
initialState = State 0 Serving Nothing

type StateRef = IORef State

initState :: IO (IORef State)
initState = newIORef initialState

getState :: IORef State -> IO State
getState = readIORef

retireStatus :: StateRef -> IO ()
retireStatus sref = do
    !_ <- atomicModifyIORef sref (\st -> (st {
              serverStatus = Retiring
            , warpThreadId = Nothing
            }, ()))
    return ()

increment :: IORef State -> IO ()
increment sref = do
    !_ <- atomicModifyIORef sref (\st -> (st {connectionCounter = connectionCounter st + 1}, ()))
    return ()

decrement :: IORef State -> IO ()
decrement sref = do
    !_ <- atomicModifyIORef sref (\st -> (st {connectionCounter = connectionCounter st - 1}, ()))
    return ()

setWarpThreadId :: IORef State -> ThreadId -> IO ()
setWarpThreadId sref tid = do
    !_ <- atomicModifyIORef sref (\st -> (st {warpThreadId = Just tid}, ()))
    return ()
