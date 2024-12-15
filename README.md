# Trivial utility for SBCL's pathname escaping

## Goals

To avoid pathname escaping by SBCL.

Especially paths like this `src/articles/[articleId]/page.js`, including path parameter in Next.js' app router.

## TODO

- [ ] invastigations
    - [ ] check the escape sequences specs described at [`sbcl/src/code/filesys.lisp`](https://github.com/sbcl/sbcl/blob/master/src/code/filesys.lisp)
        - NOTE: that spec says it may be not same that codes.
    - [ ] how that escaping behave on Windows, mac or other platforms?
        - MEMO: I can say that escaping is applied anywhare if a Next.js parameter path is escaped in not unix platform (e.g. Windows)
- [ ] implementation
    - [ ] think API for that escaping (I think it maybe one function minimally)
    - [ ] make soem utilities

## Author

- t-sin (<shinichi.tanaka45@gmail.com>)

## License

This project is licensed under the MIT license.
