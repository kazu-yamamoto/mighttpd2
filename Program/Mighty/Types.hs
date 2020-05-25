{-# LANGUAGE CPP #-}

module Program.Mighty.Types where

#ifdef DHALL
import GHC.Natural (Natural, naturalToInt)
#else
type Natural = Int

naturalToInt :: Natural -> Int
naturalToInt = id
#endif
