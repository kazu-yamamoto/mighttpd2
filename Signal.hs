module Signal where

import Control.Exception (SomeException, catch)
import System.Posix.Signals
import System.Posix
import Prelude hiding (catch)

----------------------------------------------------------------

sigStop :: Signal
sigStop   = sigTERM

sigReload :: Signal
sigReload = sigHUP

sigRetire :: Signal
sigRetire = sigQUIT

sigInfo :: Signal
sigInfo   = sigINT

----------------------------------------------------------------

sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `catch` ignore

setHandler :: Signal -> Handler -> IO ()
setHandler sig func = installHandler sig func Nothing >> return ()

ignoreSigChild :: IO ()
ignoreSigChild = setHandler sigCHLD Ignore

----------------------------------------------------------------

ignore :: SomeException -> IO ()
ignore _ = return ()
