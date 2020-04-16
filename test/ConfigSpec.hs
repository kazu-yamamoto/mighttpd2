{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec where

import Program.Mighty
import Test.Hspec

spec :: Spec
spec = do
    describe "parseConfig" $ do
        it "parses example.conf correctly" $ do
            res <- parseOption "conf/example.conf" "foo"
            res `shouldBe` ans

    describe "parseDhall" $ do
        it "parses example.dhall correctly" $ do
            res <- parseOptionDhall "./conf/example.dhall"
            res `shouldBe` ans

        it "parses literal dhall correctly" $ do
            res <- parseOptionDhall "let x = ./conf/example.dhall in x"
            res `shouldBe` ans

ans :: Option
ans = Option 
    { opt_port = 80
    , opt_host = "*"
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
    , opt_fd_cache_duration = 10
    , opt_server_name = "foo"
    , opt_routing_file = Nothing
    , opt_tls_port = 443
    , opt_tls_cert_file = "cert.pem"
    , opt_tls_chain_files = "chain.pem"
    , opt_tls_key_file = "privkey.pem"
    , opt_service = 0
    , opt_report_file = "/tmp/mighty_report"
    , opt_proxy_timeout = 0
    }
