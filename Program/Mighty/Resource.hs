module Program.Mighty.Resource (
    amIrootUser
  , setGroupUser
  , unlimit
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Program.Mighty.Exception
import System.Posix

----------------------------------------------------------------

-- | Checking if this process has the root privilege.
amIrootUser :: IO Bool
amIrootUser = (== 0) <$> getRealUserID

----------------------------------------------------------------

-- | Setting user and group.
setGroupUser :: String -- ^ User
             -> String -- ^ Group
             -> IO ()
setGroupUser user group = do
    root <- amIrootUser
    when root $ do
        getGroupEntryForName group >>= setGroupID . groupID
        getUserEntryForName user >>= setUserID . userID

----------------------------------------------------------------

-- | Set the limit of open files to 10,000.
unlimit :: IO ()
unlimit = handle ignore $ do
    hard <- hardLimit <$> getResourceLimit ResourceOpenFiles
    let lim = if hard == ResourceLimitInfinity then
                  ResourceLimits (ResourceLimit 10000) hard
                else
                  ResourceLimits hard hard
    setResourceLimit ResourceOpenFiles lim
