{-# LANGUAGE CPP #-}

module Program.Mighty.Resource (
    amIrootUser,
    setGroupUser,
    unlimit,
) where

import Control.Exception
import System.Posix

----------------------------------------------------------------

-- | Checking if this process has the root privilege.
amIrootUser :: IO Bool
amIrootUser = (== 0) <$> getRealUserID

----------------------------------------------------------------

-- | Setting user and group.
setGroupUser
    :: String
    -- ^ User
    -> String
    -- ^ Group
    -> IO Bool
setGroupUser user group = do
    root <- amIrootUser
    if root
        then do
            getGroupEntryForName group >>= setGroupID . groupID
            getUserEntryForName user >>= setUserID . userID
            return True
        else
            return False

----------------------------------------------------------------

-- | Set the limit of open files.
unlimit :: Integer -> IO ()
unlimit limit = handle (\(SomeException _) -> return ()) $ do
    hard <- hardLimit <$> getResourceLimit ResourceOpenFiles
    let lim =
            if hard == ResourceLimitInfinity
                then
                    ResourceLimits (ResourceLimit limit) hard
                else
                    ResourceLimits hard hard
    setResourceLimit ResourceOpenFiles lim
