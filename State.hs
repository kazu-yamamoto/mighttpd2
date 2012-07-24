module State where

import Control.Concurrent
import Data.IORef
import System.IO
import Utils

data Status = Serving | Retiring deriving (Eq, Show)

data State = State {
    connectionCounter :: !Int
  , serverStatus      :: !Status
  , warpThreadId      :: !(Maybe ThreadId)
  , reportHandle      :: !Handle
  }

initialState :: Handle -> State
initialState = State 0 Serving Nothing

type StateRef = IORef State

initState :: Handle -> IO (IORef State)
initState rpthdl = newIORef $ initialState rpthdl

getState :: IORef State -> IO State
getState = readIORef

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
