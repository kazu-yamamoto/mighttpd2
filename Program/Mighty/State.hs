module Program.Mighty.State (
  -- * Types
    Status(..)
  , State(..)
  , Two(..)
  , Stater
  -- * Creating Starter
  , initStater
  -- * Accessing Starter
  , getConnectionCounter
  , getServerStatus
  , isRetiring
  -- * Modifying Starter
  , increment
  , decrement
  , setMyWarpThreadId
  , addAnotherWarpThreadId
  , goRetiring
  -- * Misc
  , ifWarpThreadsAreActive
  ) where

import Control.Applicative
import Control.Concurrent
import Data.IORef
import Program.Mighty.IORef

----------------------------------------------------------------

-- | Server status
data Status = Serving | Retiring deriving (Eq, Show)

-- | FIXME
data Two a = Zero | One a | Two a a

-- | FIXME
data State = State {
    connectionCounter :: !Int
  , serverStatus      :: !Status
  , warpThreadId      :: !(Two ThreadId)
  }

initialState :: State
initialState = State 0 Serving Zero

----------------------------------------------------------------

-- | Reference to a server state.
newtype Stater = Stater (IORef State)

-- | Creating a new 'Stater'.
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
      , warpThreadId = Zero
      }

----------------------------------------------------------------

getWarpThreadId :: Stater -> IO (Two ThreadId)
getWarpThreadId (Stater sref) = warpThreadId <$> readIORef sref

setWarpThreadId :: Stater -> Two ThreadId -> IO ()
setWarpThreadId (Stater sref) ttids =
    strictAtomicModifyIORef sref $ \st -> st {
        warpThreadId = ttids
      }

setMyWarpThreadId :: Stater -> IO ()
setMyWarpThreadId stt = do
    myid <- myThreadId
    setWarpThreadId stt (One myid)

addAnotherWarpThreadId :: Stater -> ThreadId -> IO ()
addAnotherWarpThreadId stt aid = do
    ttids <- getWarpThreadId stt
    case ttids of
        One tid -> setWarpThreadId stt (Two tid aid)
        _       -> undefined -- FIXME

-- | If Warp threads are active, first terminate them and
--   run new 'IO'.
ifWarpThreadsAreActive :: Stater -> IO () -> IO ()
ifWarpThreadsAreActive stt act = do
    ttids <- getWarpThreadId stt
    case ttids of
        Zero -> return ()
        One tid -> do
            killThread tid
            act
        Two tid1 tid2 -> do
            killThread tid1
            killThread tid2
            act
