{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec where

import Control.Exception (bracket_)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Program.Mighty
import Test.Hspec

spec :: Spec
spec = do
    describe "parseConfig" $ do
        it "parses example.conf correctly" $ do
            res <- parseOption "conf/example.conf"
            res{opt_server_name = "foo"} `shouldBe` ans

        it "accepts numbers as strings and ignores empty addresses" $ do
            res <- withConfig "User: 1000\nQuic_Addr: 0.0.0.0,\n" parseOption
            opt_user res `shouldBe` "1000"
            opt_quic_addr res `shouldBe` ["0.0.0.0"]
        it "rejects a value with a wrong type" $ do
            withConfig "Port: http\n" parseOption `shouldThrow` anyIOException
        it "ignores unknown keys" $ do
            res <- withConfig "Prot: 8080\n" parseOption
            opt_port res `shouldBe` opt_port defaultOption

#ifdef DHALL
    describe "parseDhall" $ do
        it "parses example.dhall correctly" $ do
            res <- parseOptionDhall "./conf/example.dhall"
            res { opt_server_name = "foo" } `shouldBe` ans
#endif

withConfig :: String -> (FilePath -> IO a) -> IO a
withConfig conf act = do
    dir <- getTemporaryDirectory
    let file = dir </> "mighty-config-spec.conf"
    bracket_ (writeFile file conf) (removeFile file) (act file)

ans :: Option
ans =
    Option
        { opt_port = 80
        , opt_host = "*"
        , opt_debug_mode = True
        , opt_user = "root"
        , opt_group = "root"
        , opt_pid_file = "/var/run/mighty.pid"
        , opt_report_file = "/tmp/mighty_report"
        , opt_logging = True
        , opt_log_file = "/var/log/mighty"
        , opt_log_file_size = 16777216
        , opt_log_backup_number = 10
        , opt_index_file = "index.html"
        , opt_index_cgi = "index.cgi"
        , opt_status_file_dir = "/usr/local/share/mighty/status"
        , opt_connection_timeout = 30
        , opt_proxy_timeout = 0
        , opt_fd_cache_duration = 10
        , opt_service = 0
        , opt_tls_port = 443
        , opt_tls_cert_file = "cert.pem"
        , opt_tls_chain_files = "chain.pem"
        , opt_tls_key_file = "privkey.pem"
        , opt_quic_addr = ["0.0.0.0", "::"]
        , opt_quic_port = 443
        , opt_quic_debug_dir = Nothing
        , opt_quic_qlog_dir = Nothing
        , opt_server_name = "foo"
        , opt_routing_file = Nothing
        }
