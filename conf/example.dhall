let default = ./configDefault.dhall
in
    (default "foo")
    //
    { opt_connection_timeout = 30
    , opt_debug_mode = True
    , opt_fd_cache_duration = 10
    , opt_group = "root"
    , opt_host = "*"
    , opt_index_cgi = "index.cgi"
    , opt_index_file = "index.html"
    , opt_log_backup_number = 10
    , opt_log_file = "/var/log/mighty"
    , opt_log_file_size = 16777216
    , opt_logging = True
    , opt_pid_file = "/var/run/mighty.pid"
    , opt_port = 80
    , opt_proxy_timeout = 0
    , opt_report_file = "/tmp/mighty_report"
    , opt_routing_file = None Text
    , opt_service = 0
    , opt_status_file_dir = "/usr/local/share/mighty/status"
    , opt_tls_cert_file = "cert.pem"
    , opt_tls_chain_files = "chain.pem"
    , opt_tls_key_file = "privkey.pem"
    , opt_tls_port = 443
    , opt_user = "root"
    } 