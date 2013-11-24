module Program.Mighty.IORef where

import Data.IORef

----------------------------------------------------------------

-- | Strict version of 'atomicModifyIORef'.
--   When modifying the IORef, calculation is delayed.
--   So, modification is quick and CAS would success.
--   After modification, calculation is forced.
--   So, no space leak.
strictAtomicModifyIORef :: IORef a -> (a -> a) -> IO ()
strictAtomicModifyIORef ref f =  do
    c <- atomicModifyIORef ref
            (\x -> let a = f x          -- Lazy application of "f"
                    in (a, a `seq` ())) -- Lazy application of "seq"
    -- The following forces "a `seq` ()", so it also forces "f x".
    c `seq` return c
