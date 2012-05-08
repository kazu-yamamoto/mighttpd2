{-# LANGUAGE OverloadedStrings #-}

module Process (getMightyPid) where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Data.Function
import Data.List
import Data.Ord
import System.Posix.Types

----------------------------------------------------------------

data PsResult = PsResult {
    uid :: ByteString
  , pid :: ProcessID
  , ppid :: ProcessID
  , command :: ByteString
  } deriving (Eq, Show)

toPsResult :: [ByteString] -> PsResult
toPsResult (a:b:c:_:_:_:_:h:_) = PsResult {
    uid     = a
  , pid     = maybe 0 (fromIntegral . fst) $ BS.readInt b
  , ppid    = maybe 0 (fromIntegral . fst) $ BS.readInt c
  , command = h
  }
toPsResult _ = PsResult "unknown" 0 0 "unknown"

----------------------------------------------------------------

runPS :: IO [PsResult]
runPS = runResourceT $
      sourceCmd "ps -ef"
   $= CB.lines
   $= CL.map BS.words
   $= CL.map toPsResult
   $= CL.filter (\ps -> "mighty" `BS.isSuffixOf` command ps)
   $$ CL.consume

----------------------------------------------------------------

findParent :: [PsResult] -> [PsResult]
findParent ps = map head
             $ filter (\xs -> length xs == 1)
             $ groupBy ((==) `on` ppid)
             $ sortBy (comparing ppid) ps

----------------------------------------------------------------

getMightyPid :: IO [ProcessID]
getMightyPid = (map pid . findParent) <$> runPS
