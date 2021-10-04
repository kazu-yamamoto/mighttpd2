## Installing Mighttpd2

Mighttpd2 is registered in [Hackage](http://hackage.haskell.org/packages/hackage.html). So, you can install Mighttpd2 with the `cabal\ command.

To use Glasgow Haskell Compiler and `cabal` command, please install ["The Haskell Platform"](http://hackage.haskell.org/platform/).

### Updating the index of Hackage

You should download the updated index of Hackage.

```shell
% cabal update
```

Note that `cabal` uses the `http_proxy` environment variable.

### Installing Mighttpd2

The following command installs mighttpd2 with HTTP, CGI, Reverse Proxy and URL rewriting enabled.

```shell
% cabal install mighttpd2
```

Note that the `-jN` option enables parallel installation to save time.

Here is a lift of installation flags:

- `tls`: TLS support
- `quic`: QUIC support
- `dhall`: Dhall configration support

To use them, type:

```shell
% cabal install --flags="tls" --flags="quic" --flags="dhall" mighttpd2
```

### Files installed

Now you can find the followings:

- `~/.cabal/bin/mighty`: Mighttpd2, an HTTP server in Haskell
- `~/.cabal/bin/mightyctl`: the command to operate Mighttpd2
- `~/.cabal/bin/mighty-mkindex`: the command to make index.html for files in a directory
- `~/.cabal/share/mighttpd-x.y.z/example.conf`: an example configuration file for Mighttpd2
- `~/.cabal/share/mighttpd-x.y.z/example.route`: an example route file from URIs to directories

If you use Mac, please check `~/Library/Haskell`.
