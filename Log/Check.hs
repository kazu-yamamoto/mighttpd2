module Log.Check (logCheck) where

import Control.Monad
import Log.Types
import System.Directory
import System.FilePath

logCheck :: LogType -> IO ()
logCheck LogNone   = return ()
logCheck LogStdout = return ()
logCheck (LogFile spec) = do
    dirExist <- doesDirectoryExist dir
    unless dirExist $ fail $ dir ++ " does not exist or is not a directory."
    dirPerm <- getPermissions dir
    unless (writable dirPerm) $ fail $ dir ++ " is not writable."
    exist <- doesFileExist file
    when exist $ do
        perm <- getPermissions file
        unless (writable perm) $ fail $ file ++ " is not writable."
  where
    file = log_file spec
    dir = takeDirectory file
