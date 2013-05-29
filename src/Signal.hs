module Signal where

import qualified Control.Exception as E
import Control.Monad
import System.Posix
import Utils

----------------------------------------------------------------

sigStop :: Signal
sigStop   = sigTERM

sigReload :: Signal
sigReload = sigHUP

sigRetire :: Signal
sigRetire = sigQUIT

sigInfo :: Signal
sigInfo   = sigUSR2

sigLogCtl :: Signal
sigLogCtl = sigUSR1

----------------------------------------------------------------

sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `E.catch` ignore

setHandler :: Signal -> Handler -> IO ()
setHandler sig func = void $ installHandler sig func Nothing

ignoreSigChild :: IO ()
ignoreSigChild = setHandler sigCHLD Ignore
