module Program.Mighty.Exception where

import Control.Exception

-- | A exception handler to ignore all exceptions.
ignore :: SomeException -> IO ()
ignore _ = return ()
