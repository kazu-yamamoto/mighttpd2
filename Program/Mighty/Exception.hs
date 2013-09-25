module Program.Mighty.Exception where

import Control.Exception

ignore :: SomeException -> IO ()
ignore _ = return ()
