-- { port : Natural
-- , host : Text
-- , debugMode : Bool
-- , user : Text
-- , group : Text
-- , pidFile : Text
-- , reportFile : Text
-- , logging : Bool
-- , logFile : Text
-- , logFileSize : Natural
-- , logBackupNumber : Natural
-- , indexFile : Text
-- , indexCgi  : Text
-- , statusFileDir : Text
-- , connectionTimeout : Natural
-- , proxyTimeout      : Natural
-- , fdCacheDuration   : Natural
-- , service : Natural
-- , tlsPort : Natural
-- , tlsCertFile   : Text
-- , tlsChainFiles : Text
-- , tlsKeyFile    : Text
-- , quicAddr : List Text
-- , quicPort : Natural
-- , quicDebugDir : Optional Text
-- , quicQlogDir  : Optional Text
-- }
{ port = 80
-- IP address or "*"
, host = "*"
, debugMode = True
-- If available, "nobody" is much more secure for user
, user  = "root"
-- If available, "nobody" is much more secure for group
, group = "root"
, pidFile    = "/var/run/mighty.pid"
, reportFile = "/tmp/mighty_report"
, logging = True
-- The directory must be writable by the user.
, logFile = "/var/log/mighty"
, logFileSize = 16777216 -- bytes
, logBackupNumber = 10
, indexFile = "index.html"
, indexCgi  = "index.cgi"
, statusFileDir = "/usr/local/share/mighty/status"
, connectionTimeout = 30 -- seconds
, proxyTimeout      = 0  -- seconds, 0 is default of http-client, ie 30 seconds
, fdCacheDuration   = 10 -- seconds
-- 0 is HTTP only
-- 1 is HTTPS only
-- 2 is for both HTTP and HTTPs
-- 3 is for HTTP, HTTPs and QUIC(HTTP/3)
, service = 0
, tlsPort = 443
-- should change this with an absolute path
, tlsCertFile   = "cert.pem"
-- should change this with an absolute path
, tlsChainFiles = "chain.pem"
-- Currently, tlsKeyFile must not be encrypted
, tlsKeyFile    = "privkey.pem"
, quicPort = 443
, quicAddr = ["0.0.0.0","::"]
, quicDebugDir = None Text
, quicQlogDir  = None Text
}
