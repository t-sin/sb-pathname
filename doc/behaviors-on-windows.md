# Behaviors on Windows

## summary

- `*` and `?` is invalid chars in paths on windows
    - so cannot create such files
    - for details, see `Appendix 1. File naming convension on Windows`
    - so cannot checkout this repository normally
        - avoid failure set `git config --global core.protectNTFS false` but cannot creat that files
- `[` tests fail
    - bucause of same escape sequence handling on UNIX

## how to avoid this

- there is no needs to consider `*` and `?`
- escape `[` but we cannot use `\[`, we can use `^[` to escape it
    - I cannot find the doc described about escaping
    - ref: <https://en.wikipedia.org/wiki/Cmd.exe>

```lisp
parse error in namestring: #\[ with no corresponding #\]
  C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt
                                                                                                    ^
   [Condition of type SB-KERNEL:NAMESTRING-PARSE-ERROR]

Restarts:
 0: [USE-VALUE] Specify a different path.
 1: [RETRY] Retry SLIME REPL evaluation request.
 2: [*ABORT] Return to SLIME's top level.
 3: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1003D50003}>)

Backtrace:
  0: (SB-IMPL::MAYBE-MAKE-PATTERN "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt" 76 99 #\^)
  1: (SB-IMPL::EXTRACT-NAME-TYPE-AND-VERSION "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt" 76 103 #\^)
  2: (SB-IMPL::PARSE-WIN32-NAMESTRING "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt" 0 103)
  3: (SB-IMPL::%PARSE-NAMESTRING "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt" NIL #P"C:/Users/winuser/.roswell/local-projects/sb-pat..
  4: (PARSE-NAMESTRING "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt" NIL #P"C:/Users/winuser/.roswell/local-projects/sb-pathname/" :S..
  5: (PATHNAME "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt")
  6: (SB-IMPL::QUERY-FILE-SYSTEM "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt" :TRUENAME NIL)
  7: (PROBE-FILE "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt")
  8: (SB-INT:SIMPLE-EVAL-IN-LEXENV (PROBE-FILE "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt") #<NULL-LEXENV>)
  9: (EVAL (PROBE-FILE "C:/Users/winuser/.roswell/local-projects/sb-pathname/test/files/03_square-braket_open_[.txt"))
 --more--
```

### Appendix 1. File naming convension on Windows

from <https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file>

> Use any character in the current code page for a name, including Unicode characters and characters in the extended character set (128–255), except for the following:
>
> The following reserved characters:
>
> - < (less than)
> - > (greater than)
> - : (colon)
> - " (double quote)
> - / (forward slash)
> - \ (backslash)
> - | (vertical bar or pipe)
> - ? (question mark)
> - * (asterisk)

## Appendix 2. how to run tests

all commands must be run in powershell.

1. (if on GCP) create & start **4GB memory** windows VM
2. install [scoop](https://scoop.sh/) (homebrew-like something)
    1. run `Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser`
    2. run `Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression`
3. install git & roswell
    1. run `scoop install git`
    2. run `scoop install roswell`
3. create & register SSH keys (to clone the repo)
4. clone the repo & set option `git config --global core.protectNTFS false`
5. install rove: `ros install rove`
6. run tests: `~/.roswell/lisp/quicklisp/rove/bin/rove sb-pathname.asd`
