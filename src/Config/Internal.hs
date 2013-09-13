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
  , opt_user = "root"
  , opt_group = "root"
  , opt_pid_file = "/var/run/mighty.pid"
  , opt_logging = True
  , opt_log_file = "/var/log/mighty"
  , opt_log_file_size = 16777216
  , opt_log_backup_number = 10
  , opt_index_file = "index.html"
  , opt_index_cgi = "index.cgi"
  , opt_status_file_dir = "/usr/local/share/mighty/status"
  , opt_connection_timeout = 30
  , opt_response_timeout = Nothing
  , opt_fd_cache_duration = 10
  , opt_server_name = programName ++ "/" ++ programVersion
  , opt_worker_processes = 1
  , opt_routing_file = Nothing
  , opt_tls_port = 443
  , opt_tls_cert_file = "certificate.pem"
  , opt_tls_key_file = "key.pem"
  , opt_service = 0
  , opt_report_file = "/tmp/mighty_report"
}

data Option = Option {
    opt_port :: !Int
  , opt_debug_mode :: !Bool
  , opt_user :: !String
  , opt_group :: !String
  , opt_pid_file :: !FilePath
  , opt_logging :: !Bool
  , opt_log_file :: !FilePath
  , opt_log_file_size :: !Int
  , opt_log_backup_number :: !Int
  , opt_index_file :: !FilePath
  , opt_index_cgi  :: !FilePath
  , opt_status_file_dir :: !FilePath
  , opt_connection_timeout :: !Int
  , opt_response_timeout :: !(Maybe Int)
  , opt_fd_cache_duration :: !Int
  , opt_server_name :: !String
  , opt_worker_processes :: !Int
  , opt_routing_file :: !(Maybe FilePath)
  , opt_tls_port :: !Int
  , opt_tls_cert_file :: !FilePath
  , opt_tls_key_file :: !FilePath
  , opt_service :: !Int
  , opt_report_file :: !FilePath
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
  , opt_index_cgi          = get "Index_Cgi" opt_index_cgi
  , opt_status_file_dir    = get "Status_File_Dir" opt_status_file_dir
  , opt_connection_timeout = get "Connection_Timeout" opt_connection_timeout
  , opt_response_timeout   = fromConf `fmap` lookup "Response_Timeout" conf
  , opt_fd_cache_duration  = get "Fd_Cache_Duration" opt_fd_cache_duration
  , opt_server_name        = get "Server_Name" opt_server_name
  , opt_worker_processes   = get "Worker_Processes" opt_worker_processes
  , opt_routing_file       = Nothing
  , opt_tls_port           = get "Tls_Port" opt_tls_port
  , opt_tls_cert_file      = get "Tls_Cert_File" opt_tls_cert_file
  , opt_tls_key_file       = get "Tls_Key_File" opt_tls_key_file
  , opt_service            = get "Service" opt_service
  , opt_report_file        = get "ReportFile" opt_report_file
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
