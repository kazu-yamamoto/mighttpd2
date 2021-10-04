---
layout: default
title: About
rank: 1
---

## About

Mighttpd2 (called mighty) is a simple but practical HTTP server
written in [Haskell](http://www.haskell.org).
It handles static files and CGI scripts.
It also provides a feature of reverse proxy and
URL rewriting with HTTP redirect.

Mighttpd2 is now implemented as a [WAI](http://hackage.haskell.org/package/wai) application using the high-performance HTTP engine, ["Warp"](http://hackage.haskell.org/package/warp). To httperf Ping-Pong benchmark, the performance of Mighttpd2 is similar to that of nginx.

[mew.org](https://mew.org/) is provided with Mighttpd2.

Here are features provided by Mighty:

- Redirection and language negotiation
- CGI: GET and POST
- ["Index of files in a directory"](https://www.mew.org/Release/) should be statically created by the "mighty-mkindex" command.
- Reverse proxy

Here are slides/articles of Mighttpd:

- ["Experience Report: Developing High Performance HTTP/2 Server in Haskell](http://www.mew.org/~kazu/doc/paper/http2-haskell-2016.pdf)
- ["Warp"](http://aosabook.org/en/posa/warp.html), The Performance of Open Source Applications
- ["Mio: A High-Performance Multicore IO Manager for GHC"](http://haskell.cs.yale.edu/wp-content/uploads/2013/08/hask035-voellmy.pdf), Haskell Symposium 2013
- ["The architecture of Mighttpd, a high performance web server"](http://www.iij.ad.jp/company/development/tech/activities/mighttpd/index.html) (in Japanese)
- ["Mighty and Yesod"](http://mew.org/~kazu/material/2012-mighty-yesod.pdf) (slides)
- ["Mighttpd - a High Performance Web Server in Haskell"](http://themonadreader.files.wordpress.com/2011/10/issue19.pdf), the Monad.Reader, Issue 19
- ["High Performance Web Server in Haskell"](http://www.mew.org/~kazu/material/2011-mighttpd.pdf) (slides)
- ["Experience on implementing a Web server in Haskell](http://www.mew.org/~kazu/material/2010-mighttpd-en.pdf) (slides)
