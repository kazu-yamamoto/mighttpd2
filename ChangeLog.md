# ChangeLog for mighttpd2

## 4.0.8

* `mightyctl inf` now lets `mighty` to display running threads to stdout.
* Removing `unliftio`.
* Fixing a bug where multiple addresses cannot be specified to
  `opt_quic_addr`.

## 4.0.7

* Using runQUICSocket. Requiring quic v0.2 implicitly.

## 4.0.6

* Fixing the "trailing" parser.

## 4.0.5

* Using session tickets instead of in-memory session DB.

## 4.0.4

* Proper RTS options.
* Fix for "-f tls" but opt_service is 0.
  [#27](https://github.com/kazu-yamamoto/mighttpd2/pull/27)

## 4.0.3

* IMPORTANT: using TLS 1.2 and TLS 1.3 only.
* IMPORTANT: stop using server push.
* Supporting QUIC v2.

## 4.0.2

* Supporting GHC 9.0.

## 4.0.1

* Including Program/Mighty/Dhall/Option.dhall.

## 4.0.0

* HTTP/3 on QUIC support: the "Service" field should be 3.
  New configration fields:
  - Quic_Addr
  - Quic_Port
  - Quic_Debug_Dir
  - Quic_Qlog_Dir
* Dhall configration support by Alex Mouton: if the suffix of first argument is ".dhall", it is parsed as Dhall. Due to Dhall limitation, it should start with "./":
  - mighty ./conf.dhall route
* Dropping capability except BIND on Linux:
  - see https://kazu-yamamoto.hatenablog.jp/entry/2020/12/04/141308
  - see https://kazu-yamamoto.hatenablog.jp/entry/2020/12/10/150731
