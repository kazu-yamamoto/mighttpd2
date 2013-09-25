{-# LANGUAGE OverloadedStrings #-}

module Program.Mighty.Process (
    getMightyPid
  , PsResult(..)
  ) where

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
    $= CL.filter mighty
    $$ CL.consume
  where
    commandName = last . split '/' . command
    mighty ps = "mighty" `BS.isInfixOf` name
             && not ("mightyctl" `BS.isInfixOf` name)
        where
          name = commandName ps

----------------------------------------------------------------

findParent :: [PsResult] -> [PsResult]
findParent ps = deleteAloneChild $ masters ++ candidates
  where
    iAmMaster p = ppid p == 1
    masters = filter iAmMaster ps
    rest    = filter (not.iAmMaster) ps
    candidates = map head
               $ filter (\xs -> length xs == 1) -- master is alone
               $ groupBy ((==) `on` ppid)
               $ sortBy (comparing ppid) rest


deleteAloneChild :: [PsResult] -> [PsResult]
deleteAloneChild [] = []
deleteAloneChild (p:ps) = p : deleteAloneChild (filter noParent ps)
  where
    parent = pid p
    noParent x = ppid x /= parent

----------------------------------------------------------------

getMightyPid :: IO [ProcessID]
getMightyPid = (map pid . findParent) <$> runPS

----------------------------------------------------------------

split :: Char -> ByteString -> [ByteString]
split _ "" = []
split c s = case BS.break (c==) s of
    ("",r)  -> split c (BS.tail r)
    (s',"") -> [s']
    (s',r)  -> s' : split c (BS.tail r)
