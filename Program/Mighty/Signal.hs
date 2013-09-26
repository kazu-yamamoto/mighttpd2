module Program.Mighty.Signal (
  -- * Signals
    sigStop
  , sigReload
  , sigRetire
  , sigInfo
  , sigLogCtl
  -- * Signal handling
  , setHandler
  -- * Misc
  , ignoreSigChild
  ) where

import Control.Monad
import System.Posix

----------------------------------------------------------------

-- | The signal to stop Mighty.
sigStop :: Signal
sigStop   = sigTERM

-- | The signal to reload a configration file.
sigReload :: Signal
sigReload = sigHUP

-- | The signal to top accepting new connections and to finish current connections.
sigRetire :: Signal
sigRetire = sigQUIT

-- | The signal to get information from Mighty.
sigInfo :: Signal
sigInfo   = sigUSR2

-- | FIXME
sigLogCtl :: Signal
sigLogCtl = sigUSR1

----------------------------------------------------------------

-- | Setting 'Handler' for 'Signal'.
setHandler :: Signal -> Handler -> IO ()
setHandler sig func = void $ installHandler sig func Nothing
