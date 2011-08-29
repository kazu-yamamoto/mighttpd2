{-
  mkindex :: Making index.html for the current directory.
-}
import Control.Applicative
import Data.Bits
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.Locale
import System.Posix.Files
import Text.Printf

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
    fileNames <- filter dotAndIndex <$> getDirectoryContents "."
    stats <- mapM getFileStatus fileNames
    let fmsls = map pp $ zip fileNames stats
        maxLen = maximum $ map (\(_,_,_,x) -> x) fmsls
        contents = concatMap (content maxLen) fmsls
    return contents
  where
    dotAndIndex x = head x /= '.' && x /= indexFile

pp :: (String,FileStatus) -> (String,String,String,Int)
pp (f,st) = (file,mtime,size,flen)
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
\<html>\n\
\<head>\n\
\<style type=\"text/css\">\n\
\<!--\n\
\body { padding-left: 10%; }\n\
\h1 { font-size: x-large; }\n\
\pre { font-size: large; }\n\
\hr { text-align: left; margin-left: 0px; width: 80% }\n\
\-->\n\
\</style>\n\
\</head>\n\
\<title>Directory contents</title>\n\
\<body>\n\
\<h1>Directory contents</h1>\n\
\<hr>\n\
\<pre>\n"

content :: Int -> (String,String,String,Int) -> String
content lim (f,m,s,len) = "<a href=\"" ++ f ++ "\">" ++ f ++ "</a>  " ++ replicate (lim - len) ' ' ++ m ++ "  " ++ s ++ "\n"

tailer :: String
tailer = "\
\</pre>\n\
\<hr>\n\
\</body>\n\
\</html>\n"
