{-# LANGUAGE CPP #-}

-- mkindex :: Making index.html for the current directory.

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Data.Bits
import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (sort)
import Data.Word (Word8)
import Foreign.Storable (peekByteOff)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import Data.Time (formatTime)
import Data.Time.Clock.POSIX
import System.Directory
import System.Posix.Files
import Text.Printf

#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

indexFile :: String
indexFile = "index.html"

main :: IO ()
main = do
    contents <- mkContents
    writeFile indexFile $ header ++ contents ++ tailer
    setFileMode indexFile mode
  where
    mode = ownerReadMode .|. ownerWriteMode .|. groupReadMode .|. otherReadMode

mkContents :: IO String
mkContents = do
    fileNames <- sort . filter dotAndIndex <$> getDirectoryContents "."
    stats <- mapM getFileStatus fileNames
    hrefs <- mapM toHref fileNames
    let fmsls = zipWith3 pp fileNames hrefs stats
        maxLen = maximum $ map (\(_,_,_,_,x) -> x) fmsls
        contents = concatMap (content maxLen) fmsls
    return contents
  where
    dotAndIndex x = take 1 x /= "." && x /= indexFile

-- | Percent-encoding the bytes of a file name for href.
toHref :: String -> IO String
toHref f = do
    enc <- getFileSystemEncoding
    GHC.withCStringLen enc f $ \(p, n) -> do
        bytes <- mapM (\i -> peekByteOff p i) [0 .. n - 1]
        return $ concatMap escape (bytes :: [Word8])
  where
    escape w
      | unreserved c = [c]
      | otherwise    = printf "%%%02X" w
      where
        c = chr $ fromIntegral w
    unreserved c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` "-._~"

htmlEscape :: String -> String
htmlEscape = concatMap esc
  where
    esc '&'  = "&amp;"
    esc '<'  = "&lt;"
    esc '>'  = "&gt;"
    esc '"'  = "&quot;"
    esc '\'' = "&#39;"
    esc c    = [c]

pp :: String -> String -> FileStatus -> (String,String,String,String,Int)
pp f href st = (htmlEscape file,ppFile href st,mtime,size,flen)
  where
    file = ppFile f st
    flen = length file
    mtime = ppMtime st
    size = ppSize st

ppFile :: String -> FileStatus -> String
ppFile f st
  | isDirectory st = f ++ "/"
  | otherwise      = f

ppMtime :: FileStatus -> String
ppMtime st = dateFormat . epochTimeToUTCTime $ st
  where
    epochTimeToUTCTime = posixSecondsToUTCTime . realToFrac . modificationTime
    dateFormat = formatTime defaultTimeLocale "%d-%b-%Y %H:%M"

ppSize :: FileStatus -> String
ppSize st
  | isDirectory st = "  - "
  | otherwise      = sizeFormat . fromIntegral . fileSize $ st
  where
    sizeFormat siz = unit siz " KMGT"
    unit _ []  = error "unit"
    unit s [u] = format s u
    unit s (u:us)
      | s >= 1024 = unit (s `div` 1024) us
      | otherwise = format s u
    format :: Integer -> Char -> String
    format = printf "%3d%c"

header :: String
header = "\
<html>\n\
<head>\n\
<style type=\"text/css\">\n\
<!--\n\
body { padding-left: 10%; }\n\
h1 { font-size: x-large; }\n\
pre { font-size: large; }\n\
hr { text-align: left; margin-left: 0px; width: 80% }\n\
-->\n\
</style>\n\
</head>\n\
<title>Directory contents</title>\n\
<body>\n\
<h1>Directory contents</h1>\n\
<hr>\n\
<pre>\n"

content :: Int -> (String,String,String,String,Int) -> String
content lim (f,h,m,s,len) = "<a href=\"./" ++ h ++ "\">" ++ f ++ "</a>  " ++ replicate (lim - len) ' ' ++ m ++ "  " ++ s ++ "\n"

tailer :: String
tailer = "\
</pre>\n\
<hr>\n\
</body>\n\
</html>\n"
