---
layout: default
title: Configration
rank: 3
---

## Configuration

[Mighttpd2 can be executed without any arguments](operation.html).

Or specify two files to Mighttpd2 at the boot time.

```shell
% mighty <config_file> <route_file>
```

You can specify how many cores are used with GHC&#8217;s RTS -N option:

```shell
% mighty <config_file> <route_file> +RTS -N4
```

### Configuration file

The old configuration style are still supported. But Mighttpd 2 version 4 or alter offically supports the new Dhall style configuration. Here is the contents of [`example.dhall`](https://github.com/kazu-yamamoto/mighttpd2/blob/master/conf/example.dhall).

```
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
, quicAddr = ["127.0.0.1","::1"]
, quicDebugDir = None Text
, quicQlogDir  = None Text
}
```

### Route file

Here is the contents of [`example.route`](https://github.com/kazu-yamamoto/mighttpd2/blob/master/conf/example.route).

```shell
# Example routing for Mighttpd 2

# Domain lists
[localhost www.example.com]

# Entries are looked up in the specified order
# All paths must end with "/"

# A path to CGI scripts should be specified with "=>"
/~alice/cgi-bin/ => /home/alice/public_html/cgi-bin/

# A path to static files should be specified with "->"
/~alice/         -> /home/alice/public_html/
/cgi-bin/        => /export/cgi-bin/

# Reverse proxy rules should be specified with ">>"
# /path >> host:port/path2
# Either "host" or ":port" can be committed, but not both.
/app/cal/        >> example.net/calendar/
# Yesod app in the same server
/app/wiki/       >> 127.0.0.1:3000/

/                -> /export/www/
```

- A route file consists of blocks.
- A block consists of a domain list and route information.
- A domain list is in form `[domain1 domain2 ...]`
- Route information consists of lines for PATHINFO mapping.
- Characters after `#` are ignored.

If `*` is specified as a domain, any `Host:` values are accepted. 

There are three operators to map PATHINFOs (URL parts after `http://domain`) to resources:

- `->`: mapping a PATHINFO of normal resource to a file/directory. (Note that `index.html` is automatically added internally for directories. You don&#8217;t have to specify `index.html` in the route file.
- `=>`: mapping a PATHINFO of CGI path to a directory which contains CGI scripts.
- `>>`: re-writing rules for reverse proxy. You can re-write domain, port and prefix of PATHINFO.
- `<<`: re-writing rules for URL rewriting with HTTP redirect. You can re-write domain, port and prefix of PATHINFO.

The routing is first-match-from-the-top basis. So, the following is wrong.

```
# Caution: this is a wrong mapping
[*]
/            -> /export/www/
/~user/      -> /home/user/public_html/
```

The right route file is like this:

```
[*]
/~user/      -> /home/user/public_html/
/            -> /export/www/
```
