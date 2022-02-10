{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Program.Mighty.Config (
  -- * Parsing a configuration file.
    parseOption
#ifdef DHALL
  , parseOptionDhall
#else
  , Natural
#endif
  -- * Creating 'Option'.
  , defaultOption
  -- * Types
  , Option(..)
  ) where

import Data.List.Split (splitOn)
import Text.Parsec
import Text.Parsec.ByteString.Lazy
#ifdef DHALL
import Data.String (fromString)
import qualified Data.Text as T
import Dhall(Generic, Natural, input, auto, FromDhall)
import qualified Program.Mighty.Dhall.Option as Do

#else
import Program.Mighty.Types
#endif

import Program.Mighty.Parser

----------------------------------------------------------------

data Option = Option {
    opt_port :: Natural
  , opt_host :: String
  , opt_debug_mode :: Bool
  , opt_user  :: String
  , opt_group :: String
  , opt_pid_file    :: FilePath
  , opt_report_file :: FilePath
  , opt_logging :: Bool
  , opt_log_file :: FilePath
  , opt_log_file_size :: Natural
  , opt_log_backup_number :: Natural
  , opt_index_file :: FilePath
  , opt_index_cgi  :: FilePath
  , opt_status_file_dir :: FilePath
  , opt_connection_timeout :: Natural
  , opt_proxy_timeout      :: Natural
  , opt_fd_cache_duration  :: Natural
  , opt_service :: Natural
  , opt_tls_port :: Natural
  , opt_tls_cert_file   :: FilePath
  , opt_tls_chain_files :: FilePath
  , opt_tls_key_file    :: FilePath
  , opt_quic_port :: Natural
  , opt_quic_debug_dir :: Maybe FilePath
  , opt_quic_qlog_dir  :: Maybe FilePath
  , opt_server_name :: String
  , opt_routing_file :: Maybe String
#ifdef DHALL
} deriving (Eq, Show, Generic)
#else
} deriving (Eq, Show)
#endif

#ifdef DHALL
instance FromDhall Option
#endif

-- | Getting a default 'Option'.
defaultOption :: Option
defaultOption = Option {
    opt_port = 8080
  , opt_host = "*"
  , opt_debug_mode = True
  , opt_user  = "root"
  , opt_group = "root"
  , opt_pid_file    = "/var/run/mighty.pid"
  , opt_report_file = "/tmp/mighty_report"
  , opt_logging = True
  , opt_log_file = "/var/log/mighty"
  , opt_log_file_size = 16777216
  , opt_log_backup_number = 10
  , opt_index_file = "index.html"
  , opt_index_cgi  = "index.cgi"
  , opt_status_file_dir = "/usr/local/share/mighty/status"
  , opt_connection_timeout = 30
  , opt_proxy_timeout      = 0
  , opt_fd_cache_duration  = 10
  , opt_service = 0
  , opt_tls_port = 443
  , opt_tls_cert_file   = "cert.pem"
  , opt_tls_chain_files = "chain.pem"
  , opt_tls_key_file    = "privkey.pem"
  , opt_quic_port = 443
  , opt_quic_debug_dir = Nothing
  , opt_quic_qlog_dir  = Nothing
  , opt_server_name = "Dummy"
  , opt_routing_file = Nothing
}

----------------------------------------------------------------
-- | Parsing a configuration file to get an 'Option'.
parseOption :: FilePath -> IO Option
parseOption file = makeOpt defaultOption <$> parseConfig file

#ifdef DHALL
parseOptionDhall :: FilePath -> IO Option
parseOptionDhall = fmap optionFromDhall . input auto . fromString

optionFromDhall :: Do.Option -> Option
optionFromDhall o = Option
  { opt_port = Do.port o
  , opt_host = T.unpack $ Do.host o
  , opt_debug_mode = Do.debugMode o
  , opt_user  = T.unpack $ Do.user o
  , opt_group = T.unpack $ Do.group o
  , opt_pid_file    = T.unpack $ Do.pidFile o
  , opt_report_file = T.unpack $ Do.reportFile o
  , opt_logging = Do.logging o
  , opt_log_file = T.unpack $ Do.logFile o
  , opt_log_file_size = Do.logFileSize o
  , opt_log_backup_number = Do.logBackupNumber o
  , opt_index_file = T.unpack $ Do.indexFile o
  , opt_index_cgi  = T.unpack $ Do.indexCgi o
  , opt_status_file_dir = T.unpack $ Do.statusFileDir o
  , opt_connection_timeout = Do.connectionTimeout o
  , opt_proxy_timeout      = Do.proxyTimeout o
  , opt_fd_cache_duration  = Do.fdCacheDuration o
  , opt_service = Do.service o
  , opt_tls_port = Do.tlsPort o
  , opt_tls_cert_file   = T.unpack $ Do.tlsCertFile o
  , opt_tls_chain_files = T.unpack $ Do.tlsChainFiles o
  , opt_tls_key_file    = T.unpack $ Do.tlsKeyFile o
  , opt_quic_port = Do.quicPort o
  , opt_quic_debug_dir = T.unpack <$> Do.quicDebugDir o
  , opt_quic_qlog_dir  = T.unpack <$> Do.quicQlogDir o
  , opt_server_name = "Dummy"
  , opt_routing_file = Nothing
}
#endif

----------------------------------------------------------------

makeOpt :: Option -> [Conf] -> Option
makeOpt def conf = Option {
    opt_port               = get "Port" opt_port
  , opt_host               = get "Host" opt_host
  , opt_debug_mode         = get "Debug_Mode" opt_debug_mode
  , opt_user               = get "User" opt_user
  , opt_group              = get "Group" opt_group
  , opt_pid_file           = get "Pid_File" opt_pid_file
  , opt_report_file        = get "Report_File" opt_report_file
  , opt_logging            = get "Logging" opt_logging
  , opt_log_file           = get "Log_File" opt_log_file
  , opt_log_file_size      = get "Log_File_Size" opt_log_file_size
  , opt_log_backup_number  = get "Log_Backup_Number" opt_log_backup_number
  , opt_index_file         = get "Index_File" opt_index_file
  , opt_index_cgi          = get "Index_Cgi" opt_index_cgi
  , opt_status_file_dir    = get "Status_File_Dir" opt_status_file_dir
  , opt_connection_timeout = get "Connection_Timeout" opt_connection_timeout
  , opt_proxy_timeout      = get "Proxy_Timeout" opt_proxy_timeout
  , opt_fd_cache_duration  = get "Fd_Cache_Duration" opt_fd_cache_duration
  , opt_service            = get "Service" opt_service
  , opt_tls_port           = get "Tls_Port" opt_tls_port
  , opt_tls_cert_file      = get "Tls_Cert_File" opt_tls_cert_file
  , opt_tls_chain_files    = get "Tls_Chain_Files" opt_tls_chain_files
  , opt_tls_key_file       = get "Tls_Key_File" opt_tls_key_file
  , opt_quic_port          = get "Quic_Port" opt_quic_port
  , opt_quic_debug_dir     = get "Quic_Debug_Dir" opt_quic_debug_dir
  , opt_quic_qlog_dir      = get "Quic_Qlog_Dir" opt_quic_qlog_dir
  , opt_server_name        = "Dummy"
  , opt_routing_file       = Nothing
  }
  where
    get k func = maybe (func def) fromConf $ lookup k conf

----------------------------------------------------------------

type Conf = (String, ConfValue)

data ConfValue = CV_Natural Natural | CV_Bool Bool | CV_String String deriving (Eq,Show)

class FromConf a where
    fromConf :: ConfValue -> a

instance FromConf Natural where
    fromConf (CV_Natural n) = n
    fromConf _ = error "fromConf int"

instance FromConf Bool where
    fromConf (CV_Bool b) = b
    fromConf _ = error "fromConf bool"

instance FromConf String where
    fromConf (CV_String s) = s
    fromConf _ = error "fromConf string"

instance FromConf (Maybe String) where
    fromConf (CV_String "") = Nothing
    fromConf (CV_String s)  = Just s
    fromConf _ = error "fromConf string"

instance FromConf [String] where
    fromConf (CV_String s)  = splitOn "," s
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
field = (,) <$> key <*> (sep *> value)

key :: Parser String
key = many1 (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") <* spcs

sep :: Parser ()
sep = () <$ char ':' *> spcs

value :: Parser ConfValue
value = choice [try cv_natural, try cv_bool, cv_string]

-- Trailing should be included in try to allow IP addresses.
cv_natural :: Parser ConfValue
cv_natural = CV_Natural . read <$> many1 digit <* trailing

cv_bool :: Parser ConfValue
cv_bool = CV_Bool True  <$ string "Yes" <* trailing <|>
          CV_Bool False <$ string "No"  <* trailing

cv_string :: Parser ConfValue
cv_string = CV_String <$> many (noneOf " \t\n") <* trailing
