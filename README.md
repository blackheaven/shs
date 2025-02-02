# shs

Simple HTTP Haskell Server

### Usage

```
$ shs --help # or nix run github:blackheaven/shs

Simple HTTP Haskell Server

Usage: shs [-h|--host ADDRESS] [-p|--port PORT] [--base-url URL] [--root PATH]
           [--sorting-order ORDER] [--sorting-field FIELD]
           [--date-format FORMAT]

Available options:
  -h,--host ADDRESS        Listening host (default: Host "127.0.0.1")
  -p,--port PORT           Listening port
  --base-url URL           Base url (default: "/")
  --root PATH              Root directory (default: ".")
  --sorting-order ORDER    Sorting order (default: asc)
  --sorting-field FIELD    Sorting field (default: name)
  --date-format FORMAT     Date format (default: absolute)
  -h,--help                Show this help text
```
