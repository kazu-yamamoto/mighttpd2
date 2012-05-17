module Signal where

import Control.Exception (SomeException, catch)
import Control.Monad
import Prelude hiding (catch)
import System.Posix

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
setHandler sig func = void $ installHandler sig func Nothing

ignoreSigChild :: IO ()
ignoreSigChild = setHandler sigCHLD Ignore

----------------------------------------------------------------

ignore :: SomeException -> IO ()
ignore _ = return ()
