module Resource (
    amIrootUser
  , setGroupUser
  , unlimit
  ) where

import Config
import Control.Applicative
import Control.Exception
import Control.Monad
import System.Posix
import Utils

----------------------------------------------------------------

amIrootUser :: IO Bool
amIrootUser = (== 0) <$> getRealUserID

setGroupUser :: Option -> IO ()
setGroupUser opt = do
    root <- amIrootUser
    when root $ do
        getGroupEntryForName (opt_group opt) >>= setGroupID . groupID
        getUserEntryForName (opt_user opt) >>= setUserID . userID

----------------------------------------------------------------

unlimit :: IO ()
unlimit = handle ignore $ do
    hard <- hardLimit <$> getResourceLimit ResourceOpenFiles
    let lim = if hard == ResourceLimitInfinity then
                  ResourceLimits (ResourceLimit 10000) hard
                else
                  ResourceLimits hard hard
    setResourceLimit ResourceOpenFiles lim
