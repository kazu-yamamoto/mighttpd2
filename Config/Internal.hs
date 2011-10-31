{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}

module Config.Internal where

import Control.Applicative hiding (many,optional,(<|>))
import Parser
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Types

----------------------------------------------------------------

defaultOption :: Option
defaultOption = Option {
    opt_port = 8080
  , opt_debug_mode = True
  , opt_user = "nobody"
  , opt_group = "nobody"
  , opt_pid_file = "/var/run/mighty.pid"
  , opt_logging = True
  , opt_log_file = "/var/log/mighty"
  , opt_log_file_size = 16777216
  , opt_log_backup_number = 10
  , opt_index_file = "index.html"
  , opt_connection_timeout = 30
  , opt_server_name = programName ++ "/" ++ programVersion
  , opt_worker_processes = 1
}

data Option = Option {
    opt_port :: !Int
  , opt_debug_mode :: !Bool
  , opt_user :: !String
  , opt_group :: !String
  , opt_pid_file :: !String
  , opt_logging :: !Bool
  , opt_log_file :: !String
  , opt_log_file_size :: !Int
  , opt_log_backup_number :: !Int
  , opt_index_file :: !String
  , opt_connection_timeout :: !Int
  , opt_server_name :: !String
  , opt_worker_processes :: !Int
} deriving (Eq,Show)

----------------------------------------------------------------

parseOption :: String -> IO Option
parseOption file = makeOpt defaultOption <$> parseConfig file

----------------------------------------------------------------

makeOpt :: Option -> [Conf] -> Option
makeOpt def conf = Option {
    opt_port               = get "Port" opt_port
  , opt_debug_mode         = get "Debug_Mode" opt_debug_mode
  , opt_user               = get "User" opt_user
  , opt_group              = get "Group" opt_group
  , opt_pid_file           = get "Pid_File" opt_pid_file
  , opt_logging            = get "Logging" opt_logging
  , opt_log_file           = get "Log_File" opt_log_file
  , opt_log_file_size      = get "Log_File_Size" opt_log_file_size
  , opt_log_backup_number  = get "Log_Backup_Number" opt_log_backup_number
  , opt_index_file         = get "Index_File" opt_index_file
  , opt_connection_timeout = get "Connection_Timeout" opt_connection_timeout
  , opt_server_name        = get "Server_Name" opt_server_name
  , opt_worker_processes   = get "Worker_Processes" opt_worker_processes
  }
  where
    get k func = maybe (func def) fromConf $ lookup k conf

----------------------------------------------------------------

type Conf = (String, ConfValue)

data ConfValue = CV_Int Int | CV_Bool Bool | CV_String String deriving (Eq,Show)

class FromConf a where
    fromConf :: ConfValue -> a

instance FromConf Int where
    fromConf (CV_Int n) = n
    fromConf _ = error "fromConf int"

instance FromConf Bool where
    fromConf (CV_Bool b) = b
    fromConf _ = error "fromConf bool"

instance FromConf String where
    fromConf (CV_String s) = s
    fromConf _ = error "fromConf string"

----------------------------------------------------------------

parseConfig :: FilePath -> IO [Conf]
parseConfig = parseFile config

----------------------------------------------------------------

config :: Parser [Conf]
config = commentLines *> many cfield <* eof
  where
    cfield = field <* commentLines

field :: Parser Conf
field = (,) <$> key <*> (sep *> value) <* trailing

key :: Parser String
key = many1 (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") <* spcs

sep :: Parser ()
sep = () <$ char ':' *> spcs

value :: Parser ConfValue
value = choice [try cv_int, try cv_bool, cv_string] <* spcs

cv_int :: Parser ConfValue
cv_int = CV_Int . read <$> many1 digit

cv_bool :: Parser ConfValue
cv_bool = CV_Bool True  <$ string "Yes" <|>
          CV_Bool False <$ string "No"

cv_string :: Parser ConfValue
cv_string = CV_String <$> many1 (noneOf " \t\n")
