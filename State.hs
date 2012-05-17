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

strictAtomicModifyIORef :: IORef a -> (a -> a) -> IO ()
strictAtomicModifyIORef ref f = do
    !_ <- atomicModifyIORef ref (\x -> let !r = f x in (r, ()))
    return ()

retireStatus :: StateRef -> IO ()
retireStatus sref =
    strictAtomicModifyIORef sref $ \st -> st {
        serverStatus = Retiring
      , warpThreadId = Nothing
      }

increment :: IORef State -> IO ()
increment sref =
    strictAtomicModifyIORef sref $ \st -> st {
        connectionCounter = connectionCounter st + 1
      }

decrement :: IORef State -> IO ()
decrement sref =
    strictAtomicModifyIORef sref $ \st -> st {
        connectionCounter = connectionCounter st - 1
      }

setWarpThreadId :: IORef State -> ThreadId -> IO ()
setWarpThreadId sref tid =
    strictAtomicModifyIORef sref $ \st -> st {
        warpThreadId = Just tid
      }
