module Program.Mighty.Resource (
    amIrootUser
  , setGroupUser
  , unlimit
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import System.Posix

----------------------------------------------------------------

amIrootUser :: IO Bool
amIrootUser = (== 0) <$> getRealUserID

setGroupUser :: String -> String -> IO ()
setGroupUser user group = do
    root <- amIrootUser
    when root $ do
        getGroupEntryForName group >>= setGroupID . groupID
        getUserEntryForName user >>= setUserID . userID

----------------------------------------------------------------

unlimit :: IO ()
unlimit = handle ignore $ do
    hard <- hardLimit <$> getResourceLimit ResourceOpenFiles
    let lim = if hard == ResourceLimitInfinity then
                  ResourceLimits (ResourceLimit 10000) hard
                else
                  ResourceLimits hard hard
    setResourceLimit ResourceOpenFiles lim

ignore :: SomeException -> IO ()
ignore _ = return ()
