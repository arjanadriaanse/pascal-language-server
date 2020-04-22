# Pascal Language Server

An [LSP](https://microsoft.github.io/language-server-protocol/) server
implementation for Pascal variants that are supported by [Free
Pascal](https://www.freepascal.org/), including Object Pascal. It uses
[CodeTools](https://wiki.lazarus.freepascal.org/Codetools) from
Lazarus as backend.

## Features

The implementation is still incomplete and unstable. Currently only
code completion is supported. Any help and feedback is welcome.

## Clients

To use the server from `lsp-mode` in Emacs, install the separate
[`lsp-pascal`](https://github.com/arjanadriaanse/lsp-pascal) module.

## Building

Requires Free Pascal Compiler version 3.2.0 and Lazarus version 2.0.8,
open the project file in Lazarus or use the commandline:

```sh
lazbuild pasls.lpi
```
