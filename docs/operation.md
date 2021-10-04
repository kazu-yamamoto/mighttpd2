---
layout: default
title: Operation
rank: 4
---

## Operation

### Executing Mighttpd2 without configuration files

If Mighttpd2 without TLS and QUIC is executed without any arguments, it runs in the debug mode
taking the current directory as document root. If executed with 
the root privilege, it binds to the port 80. 

```shell
% sudo mighty
http://localhost/
```

Otherwise, it binds to the port 8080.

```shell
% mighty
http://localhost:8080/
```

### Executing Mighttpd2 with configuration files

If you want to use port less than or equal to 1028, typically port 80, you need the root privilege.

```shell
% sudo mighty <config_file> <route_file>
Serving on port 80 and detaching this terminal...
(If errors occur, they will be written in "/tmp/mighty_report".)
```

The suffix of `<config_file>` should be `.dhall`. Due to Dhall limitation, its path shoud start with `./`. (E.g. `./example.dhall`)

Otherwise (e.g. port 8000), you can execute Mighttpd2 without the root privilege.

```shell
% mighty <config_file> <route_file>
Serving on port 8000 and detaching this terminal...
(If errors occur, they will be written in "/tmp/mighty_report".)
```

The process creates a file which contains is its process ID. `pidFile` in <config_file> is used for the file name.

### Specifying how many cores are used

To specify how many cores are used, use GHC's RTS -Nx option. The following
is an example to use 4 cores:

```shell
% sudo mighty <config_file> <route_file> +RTS -N4
Serving on port 80 and detaching this terminal...
(If errors occur, they will be written in "/tmp/mighty_report".)
```

If you want to use all cores, specify -N without the number:

```shell
% sudo mighty <config_file> <route_file> +RTS -N
Serving on port 80 and detaching this terminal...
(If errors occur, they will be written in "/tmp/mighty_report".)
```

### Trouble

Errors of execution time are written to `/tmp/mighty_report`. Also, this file contains some status information.

### Stopping

Use `mightyctl stop`.

```shell
% sudo mightyctl stop
```

Or

```shell
% mightyctl stop
```

### Reloading the route file

Use `mightyctl reload`.

```shell
% sudo mightyctl reload
```

Or

```shell
% mightyctl reload
```

You can see what happened in `/tmp/mighty_report`:

```shell
18 May 2012 11:34:04: pid = 21476: Mighty started
18 May 2012 11:35:02: pid = 21476: Mighty reloaded
```

### Upgrading

Use `mightclt retire` to let the current processes to enter the retiring mode.
In this mode, Mighttpd 2 stops accepting new connections but keeps serving the existing connections. After all existing connections are completed, Mighttpd 2 disappears.

```shell
sudo mightyctl retire; sudo <new-mighty> conf route
```

or

```shell
mightyctl retire; <new-mighty> conf route
```

You can see what happened in `/tmp/mighty_report`:

```shell
21 May 2012 10:59:39: pid = 21476: Mighty retiring
21 May 2012 10:59:39: pid = 29837: Mighty started
21 May 2012 10:59:39: pid = 21479: Mighty retired
```

### Watching

If you want to know how many connections exist, use `mightyctl info`. If both serving Mighttpd2 and retiring Mighttpd2 exist, you need to specify one of process IDs.

```shell
% sudo mightyctl info
```

or

```shell
% mightyctl info
```

You can see the reutls in `/tmp/mighty_report`:

```shell
19 May 2012 18:19:41: pid = 21479: Retiring: # of connections = 9
```

### Making index for files in a directory

Mighttpd2 does not dynamically create `index.html` for a directory even if it is not contains `index.html`. This is for security reasons. If you want to open files in a directory, you need to create `index.html` by the `mighty-mkindex` command beforehand.

```shell
% cd <target_directory>
% mighty-mkindex
```
