# Trivial utility for SBCL's pathname escaping

This library provides a small utilities to handle SBCL's glob pattern escaping.

## Motivation

SBCL replaces automtically some characters in pathname like `[`. This SBCLs behavior mades inconviniences in some cases e.g. reading files in a [Next.js](https://nextjs.org) project.

Handling this issue is very useful especially text editor written in Common Lisp.

## TODO

- [ ] invastigations
    - [x] check the escape sequences specs described at [`sbcl/src/code/filesys.lisp`](https://github.com/sbcl/sbcl/blob/master/src/code/filesys.lisp)
        - NOTE: that spec says it may be not same that codes.
        - [resulf of reading spec/checking behaviors](doc/filesys-escape-sequences.md)
            - it seems WIP behaviors for UNIX glob support
    - [ ] how that escaping behave on Windows, mac or other platforms?
        - MEMO: I can say that escaping is applied anywhare if a Next.js parameter path is escaped in not unix platform (e.g. Windows)
        - results:
            - [x] macos: same behavor
            - [ ] windows: now checking...
- [ ] implementation
    - [ ] think API for that escaping (I think it maybe one function minimally)
    - [ ] make some utilities

- t-sin (<shinichi.tanaka45@gmail.com>)

## License

This project is licensed under the MIT license.
