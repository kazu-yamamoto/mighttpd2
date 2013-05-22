module State (
    Status(..)
  , Stater
  , initStater
  , getConnectionCounter
  , increment
  , decrement
  , isRetiring
  , goRetiring
  , getServerStatus
  , getWarpThreadId
  , setWarpThreadId
  ) where

import Control.Applicative
import Control.Concurrent
import Data.IORef
import Utils

----------------------------------------------------------------

data Status = Serving | Retiring deriving (Eq, Show)

data State = State {
    connectionCounter :: !Int
  , serverStatus      :: !Status
  , warpThreadId      :: !(Maybe ThreadId)
  }

initialState :: State
initialState = State 0 Serving Nothing

----------------------------------------------------------------

newtype Stater = Stater (IORef State)

initStater :: IO Stater
initStater = Stater <$> newIORef initialState

----------------------------------------------------------------

getConnectionCounter :: Stater -> IO Int
getConnectionCounter (Stater sref) = connectionCounter <$> readIORef sref

increment :: Stater -> IO ()
increment (Stater sref) =
    strictAtomicModifyIORef sref $ \st -> st {
        connectionCounter = connectionCounter st + 1
      }

decrement :: Stater -> IO ()
decrement (Stater sref) =
    strictAtomicModifyIORef sref $ \st -> st {
        connectionCounter = connectionCounter st - 1
      }

----------------------------------------------------------------

getServerStatus :: Stater -> IO Status
getServerStatus (Stater sref) = serverStatus <$> readIORef sref

isRetiring :: Stater -> IO Bool
isRetiring stt = (== Retiring) <$> getServerStatus stt

goRetiring :: Stater -> IO ()
goRetiring (Stater sref) =
    strictAtomicModifyIORef sref $ \st -> st {
        serverStatus = Retiring
      , warpThreadId = Nothing
      }

----------------------------------------------------------------

getWarpThreadId :: Stater -> IO (Maybe ThreadId)
getWarpThreadId (Stater sref) = warpThreadId <$> readIORef sref

setWarpThreadId :: Stater -> ThreadId -> IO ()
setWarpThreadId (Stater sref) tid =
    strictAtomicModifyIORef sref $ \st -> st {
        warpThreadId = Just tid
      }
