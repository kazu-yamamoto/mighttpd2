module Program.Mighty.Signal (
  -- * Signals
    sigStop
  , sigReload
  , sigRetire
  , sigInfo
  , sigLogCtl
  -- * Signal handling
  , sendSignal
  , setHandler
  -- * Misc
  , ignoreSigChild
  ) where

import qualified Control.Exception as E
import Control.Monad
import Program.Mighty.Exception
import System.Posix

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
