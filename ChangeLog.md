# ChangeLog for mighttpd2

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
