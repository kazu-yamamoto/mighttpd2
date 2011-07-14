module Log.Stdout (stdoutLoggerInit) where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Log.Apache
import Log.Date
import Log.Types

stdoutLoggerInit :: IO Logger
stdoutLoggerInit = stdoutLogger <$> dateInit

stdoutLogger :: DateRef -> Logger
stdoutLogger dateref req status msiz = do
    date <- getDate dateref
    BS.putStr $ BS.concat $ map toBS $ apacheFormat date req status msiz
  where
    toBS (LS s) = pack s
    toBS (LB s) = s
