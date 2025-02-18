# Trivial utility for SBCL's pathname escaping

This library provides a small utilities to handle SBCL's glob pattern escaping.

## Usage (WIP, ja)
### `sb-path:to-sb-pathname pathname`
### `sb-path:from-sb-pathname`

## Motivation

Originally, fix an error occurred by opening a file like `nextjs-proj/src/pages/posts/[postId]/index.tsx` with [Lem](https://github.com/lem-project/lem) the editor written in Common Lisp.

It seems that SBCL replaces automtically some characters in pathname like `[`. **But maybe it's wrong...??

## TODO

- [x] invastigations
    - [x] check the escape sequences specs described at [`sbcl/src/code/filesys.lisp`](https://github.com/sbcl/sbcl/blob/master/src/code/filesys.lisp)
        - NOTE: that spec says it may be not same that codes.
        - [resulf of reading spec/checking behaviors](doc/filesys-escape-sequences.md)
            - it seems WIP behaviors for UNIX glob support
    - [x] how that escaping behave on Windows, mac or other platforms?
        - MEMO: I can say that escaping is applied anywhare if a Next.js parameter path is escaped in not unix platform (e.g. Windows)
        - results: issued problem occurs almost platform (especially Windows) but some characters cannot be used in path string in OS reason
            - [x] macos: same behavor
            - [x] windows: mostly same behavior, but diffrerent little. see [windows behaviors](doc/behaviors-on-windows.md)
- [x] implementation
    - [x] think API for that escaping (I think it maybe one function minimally)
        - two functions convert from/to escaped pathname
    - [x] make some utilities
        - [x] `to-sb-pathname` converts to avoid UNIX wildcard patterns
        - [x] `from-sb-pathname` is an inversion of `to-sb-pathname`
- [ ] refinements or rethink
    - [ ] test cases (other platform-special chars, other pathname functions)
    - [ ] applicable for cl:pathname type
    - [ ] interface

## Author

- t-sin (<shinichi.tanaka45@gmail.com>)

## License

This project is licensed under the MIT license.
