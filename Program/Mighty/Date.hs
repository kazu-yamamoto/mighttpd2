-- |
-- Formatting time is slow.
-- This package provides mechanisms to cache formatted date.
module Program.Mighty.Date(
  -- * Types
    DateCacheConf(..)
  , DateCacheGetter
  , DateCacheUpdater
  -- * Date cacher
  , clockDateCacher
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.IORef

type DateCacheGetter = IO ByteString
type DateCacheUpdater = IO ()

data DateCache t = DateCache {
    timeKey :: !t
  , formattedDate :: !ByteString
  } deriving (Eq, Show)

data DateCacheConf t = DateCacheConf {
    -- | A function to get a time. E.g 'epochTime' and 'getCurrentTime'.
    getTime :: IO t
    -- | A function to format a time.
  , formatDate :: t -> IO ByteString
  }

newDate :: DateCacheConf t -> t -> IO (DateCache t)
newDate setting tm = DateCache tm <$> formatDate setting tm

-- |
-- Date cacher which gets a time and formatted it every second.
-- This returns a getter.
clockDateCacher :: Eq t => DateCacheConf t -> IO (DateCacheGetter, DateCacheUpdater)
clockDateCacher setting = do
    ref <- getTime setting >>= newDate setting >>= newIORef
    return $! (getter ref, clock ref)
  where
    getter ref = formattedDate <$> readIORef ref
    clock ref = do
        tm <- getTime setting
        date <- formatDate setting tm
        let new = DateCache {
                timeKey = tm
              , formattedDate = date
              }
        writeIORef ref new
