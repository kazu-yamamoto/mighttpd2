{-# LANGUAGE CPP #-}

module Program.Mighty.Types (Natural, naturalToInt) where

#ifdef DHALL
import GHC.Natural (Natural, naturalToInt)
#else
type Natural = Int

naturalToInt :: Natural -> Int
naturalToInt = id
#endif
