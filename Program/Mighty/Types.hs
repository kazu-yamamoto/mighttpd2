{-# LANGUAGE CPP #-}

module Program.Mighty.Types (Natural, naturalToInt) where

#ifdef DHALL
import GHC.Natural (Natural, naturalToWord)

naturalToInt :: Natural -> Int
naturalToInt = fromIntegral . naturalToWord
#else
type Natural = Int

naturalToInt :: Natural -> Int
naturalToInt = id
#endif
