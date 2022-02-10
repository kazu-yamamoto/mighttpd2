{ port : Natural
, host : Text
, debugMode : Bool
, user  : Text
, group : Text
, pidFile    : Text
, reportFile : Text
, logging : Bool
, logFile : Text
, logFileSize : Natural
, logBackupNumber : Natural
, indexFile : Text
, indexCgi  : Text
, statusFileDir : Text
, connectionTimeout : Natural
, proxyTimeout      : Natural
, fdCacheDuration   : Natural
, service : Natural
, tlsPort : Natural
, tlsCertFile   : Text
, tlsChainFiles : Text
, tlsKeyFile    : Text
, quicPort : Natural
, quicDebugDir : Optional Text
, quicQlogDir  : Optional Text
}