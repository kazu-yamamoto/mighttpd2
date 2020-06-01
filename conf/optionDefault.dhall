let Option = ../Program/Mighty/Dhall/Option.dhall
in
  λ(svrnm : Text)
    → { connectionTimeout = 30
    , debugMode = True
    , fdCacheDuration = 10
    , group = "root"
    , host = "*"
    , indexCgi = "index.cgi"
    , indexFile = "index.html"
    , logBackupNumber = 10
    , logFile = "/var/log/mighty"
    , logFileSize = 16777216
    , logging = True
    , pidFile = "/var/run/mighty.pid"
    , port = 8080
    , proxyTimeout = 0
    , reportFile = "/tmp/mighty_report"
    , routingFile = None Text
    , serverName = svrnm
    , service = 0
    , statusFileDir = "/usr/local/share/mighty/status"
    , tlsCertFile = "cert.pem"
    , tlsChainFiles = "chain.pem"
    , tlsKeyFile = "privkey.pem"
    , tlsPort = 443
    , user = "root"
    } : Option