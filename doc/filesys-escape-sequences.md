# Checking escape sequences specs and actual behaviros

>     - [ ] check the escape sequences specs described at [`sbcl/src/code/filesys.lisp`](https://github.com/sbcl/sbcl/blob/master/src/code/filesys.lisp)
>         - NOTE: that spec says it may be not same that codes.
>         - [reading memo](filesys-escape-sequences.md)
>     - [ ] how that escaping behave on Windows, mac or other platforms?
>         - MEMO: I can say that escaping is applied anywhare if a Next.js parameter path is escaped in

## specs in SBCL source code

from [sbcl/src/code/filesys.lisp at 2025-02-16](https://github.com/sbcl/sbcl/blob/2cc4b19e76f5389ddbdd7d3a3f48f1a26f516815/src/code/filesys.lisp#L40)

```
;;;; Unix pathname host support

;;; FIXME: the below shouldn't really be here, but in documentation
;;; (chapter 19 makes a lot of requirements for documenting
;;; implementation-dependent decisions), but anyway it's probably not
;;; what we currently do.
;;;
;;; Unix namestrings have the following format:
;;;
;;; namestring := [ directory ] [ file [ type [ version ]]]
;;; directory := [ "/" ] { file "/" }*
;;; file := [^/]*
;;; type := "." [^/.]*
;;; version := "." ([0-9]+ | "*")
;;;
;;; Note: this grammar is ambiguous. The string foo.bar.5 can be
;;; parsed as either just the file specified or as specifying the
;;; file, type, and version. Therefore, we use the following rules
;;; when confronted with an ambiguous file.type.version string:
;;;
;;; - If the first character is a dot, it's part of the file. It is not
;;; considered a dot in the following rules.
;;;
;;; - Otherwise, the last dot separates the file and the type.
;;;
;;; Wildcard characters:
;;;
;;; If the directory, file, type components contain any of the
;;; following characters, it is considered part of a wildcard pattern
;;; and has the following meaning.
;;;
;;; ? - matches any one character
;;; * - matches any zero or more characters.
;;; [abc] - matches any of a, b, or c.
;;; {str1,str2,...,strn} - matches any of str1, str2, ..., or strn.
;;;   (FIXME: no it doesn't)
;;;
;;; Any of these special characters can be preceded by an escape
;;; character to cause it to be treated as a regular character.
```

## summary (ja)

- UNIXのファイルパスは上のBNFっぽいようなやつ
    - `namestring := [ directory ] [ file [ type [ version ]]]`
- ただし曖昧な構文である
    - `foo.bar.5`というファイルパスは2つの解釈が可能
        - ファイル名 `foo.bar.5` (ファイル名は`.`を許容しているため)
        - ファイル名が`foo`、タイプ名が`bar`、バージョン`5`のファイル
    - なので(ここでは)続くルールを適用する
        - 1文字目が`.`: `.`はファイル名の一部。ただし末尾のドットは次の規則に従う
        - 末尾が`.`: ファイル名とタイプのセパレータと認識する
    - 例をいくつか考える
        - `.`: ファイル名が`.`、タイプは空
        - `.foo.bar.2000`: ファイル名が`.foo`、タイプが`bar`、バージョンが`2000`
        - `foo.bar.2000.`: ファイル名が`foo.bar.2000`、タイプが空、バージョンも空
- ワイルドカード文字列
    - ディレクトリやファイル、タイプはワイルドカードパターンを表す文字列を含んでよい
    - このリポジトリで問題としている、SBCLがたぶん勝手にエスケープしてくれてるやつ?
    - ちなみにワイルドカードの一覧は…
        - `?`: 任意の1文字
        - `*`: 任意の0文字以上の文字列
        - `[abc]`: `a`か`b`か`b`、これはおそらく文字 (文字列ではなく)
        - `{str1,str2,...,strn}`:str1からstrnのどれかの文字列
            - これはSBCLではやってないっぽい。
- この挙動、もともとはUNIXの[glob](https://en.wikipedia.org/wiki/Glob_(programming))に将来対応できるようにするための暫定挙動な気がする
    - ところで現在のglobには`{s1,s2,..,sn}`のパターンはないっぽい。このコードの実装当時にそういうUNIXがあったのかな？
- 対応方法としては、SBCLの自動エスケープ処理をすり抜けるように自動エスケープする関数を提供すればよさそう
    - これが必要ない場合もあるので、利用を任意にできるようライブラリで提供する
    - Windowsやmacでどんな挙動するかわからないけど、これもこのライブラリに閉じ込めればいいかも
        - OS毎に挙動が変わらないなら`trivial-sbcl-glob`みたいなライブラリ名かなー

## summary (en)

- that syntax is ambigous
    - so some additional rules are introduced for SBCL
- I think this is a work-in-progress behaviors for UNIX glob support
- I cannot checked behaviors on Windows and mac
    - it will affect to the name of this library
    - if it's UNIX only, the name is like `trivial-sbcl-glob`
    - otherwise, `trivial-sbcl-path-ecaping`? I don't know

## actual behaviors

```lisp
CL-USER> (list (lisp-implementation-type) (lisp-implementation-version))
("SBCL" "2.4.7")
```

### `[abc]` patterns


```lisp
CL-USER> (defun describe-error-when-open-file (namestring)
           (handler-case (with-open-file (in namestring))
             (t (s) (format t "~s~%" (describe s)))))
DESCRIBE-ERROR-WHEN-OPEN-FILE
CL-USER> (describe-error-when-open-file "next_proj/src/articles/[id]/index.ts")
#<SB-KERNEL:NO-NATIVE-NAMESTRING-ERROR "of the directory segment ~S." {100273DC53}>
  [condition]

Slots with :INSTANCE allocation:
  FORMAT-CONTROL                 = "of the directory segment ~S."
  FORMAT-ARGUMENTS               = (#<SB-IMPL::PATTERN (:CHARACTER-SET . "id")>)
  PATHNAME                       = #P"/home/grey/Dropbox/code/sbcl-unix-pathname/next_proj/src/articles/[id]/index.ts"
  PROBLEM                        = "does not have a native namestring"
NIL
NIL
CL-USER> (describe-error-when-open-file "next_proj/src/articles/\[id]/index.ts")
#<SB-KERNEL:NO-NATIVE-NAMESTRING-ERROR "of the directory segment ~S." {10027774E3}>
  [condition]

Slots with :INSTANCE allocation:
  FORMAT-CONTROL                 = "of the directory segment ~S."
  FORMAT-ARGUMENTS               = (#<SB-IMPL::PATTERN (:CHARACTER-SET . "id")>)
  PATHNAME                       = #P"/home/grey/Dropbox/code/sbcl-unix-pathname/next_proj/src/articles/[id]/index.ts"
  PROBLEM                        = "does not have a native namestring"
NIL
NIL
CL-USER> (describe-error-when-open-file "next_proj/src/articles/\\[id]/index.ts")
#<FILE-DOES-NOT-EXIST {10027EB2F3}>
  [condition]

Slots with :INSTANCE allocation:
  PATHNAME                       = #P"/home/grey/Dropbox/code/sbcl-unix-pathname/next_proj/src/articles/\\[id]/index.ts"
  FORMAT-CONTROL                 = NIL
  FORMAT-ARGUMENTS               = NIL
  MESSAGE                        = "No such file or directory"
NIL
NIL
```

### `{s1,s2,s3}` patterns

```lisp
CL-USER> (describe-error-when-open-file "{foo,bar,buzz}.lisp")
#<FILE-DOES-NOT-EXIST {1002A49313}>
[condition]

Slots with :INSTANCE allocation:
PATHNAME                       = #P"/home/grey/Dropbox/code/sbcl-unix-pathname/{foo,bar,buzz}.lisp"
FORMAT-CONTROL                 = NIL
FORMAT-ARGUMENTS               = NIL
MESSAGE                        = "No such file or directory"
NIL
NIL
```

### `?` patterns

```lisp
CL-USER> (describe-error-when-open-file "aaa?.md")
#<SB-KERNEL:NO-NATIVE-NAMESTRING-ERROR "of the ~S component ~S." {1002A4B6A3}>
  [condition]

Slots with :INSTANCE allocation:
  FORMAT-CONTROL                 = "of the ~S component ~S."
  FORMAT-ARGUMENTS               = (:NAME #<SB-IMPL::PATTERN "aaa" :SINGLE-CHAR-WILD>)
  PATHNAME                       = #P"/home/grey/Dropbox/code/sbcl-unix-pathname/aaa?.md"
  PROBLEM                        = "does not have a native namestring"
NIL
NIL
```

### `*` patterns

```lisp
CL-USER> (describe-error-when-open-file "aaa*aaa.md")
#<SB-KERNEL:NO-NATIVE-NAMESTRING-ERROR "of the ~S component ~S." {1002A4EF83}>
  [condition]

Slots with :INSTANCE allocation:
  FORMAT-CONTROL                 = "of the ~S component ~S."
  FORMAT-ARGUMENTS               = (:NAME #<SB-IMPL::PATTERN "aaa" :MULTI-CHAR-WILD "aaa">)
  PATHNAME                       = #P"/home/grey/Dropbox/code/sbcl-unix-pathname/aaa*aaa.md"
  PROBLEM                        = "does not have a native namestring"
NIL
NIL
```

## backtraces when `NO-NATIVE_NAMESTRING-ERROR`

```
The pathname
#P"/home/grey/Dropbox/code/sbcl-unix-pathname/./READM?.md"
does not have a native namestring because
of the :NAME component #<SB-IMPL::PATTERN "READM"
                                          :SINGLE-CHAR-WILD>.
   [Condition of type SB-KERNEL:NO-NATIVE-NAMESTRING-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD tid=46498 "repl-thread" RUNNING {1000BA0003}>)

Backtrace:
  0: (SB-KERNEL:NO-NATIVE-NAMESTRING-ERROR #P"/home/grey/Dropbox/code/sbcl-unix-pathname/./READM?.md" "of the ~S component ~S." :NAME #<SB-IMPL::PATTERN ("READM" :SINGLE-CHAR-WILD)>)
  1: (SB-IMPL::UNPARSE-NATIVE-PHYSICAL-FILE #P"/home/grey/Dropbox/code/sbcl-unix-pathname/./READM?.md")
  2: (SB-IMPL::UNPARSE-NATIVE-UNIX-NAMESTRING #P"/home/grey/Dropbox/code/sbcl-unix-pathname/./READM?.md" T)
  3: (OPEN "./READM?.md" :DIRECTION :INPUT :ELEMENT-TYPE CHARACTER :IF-EXISTS NIL :IF-DOES-NOT-EXIST NIL :EXTERNAL-FORMAT :DEFAULT :CLASS SB-SYS:FD-STREAM)
  4: ((LAMBDA ()))
  5: (SB-INT:SIMPLE-EVAL-IN-LEXENV (WITH-OPEN-FILE (IN "./READM?.md")) #<NULL-LEXENV>)
  6: (EVAL (WITH-OPEN-FILE (IN "./READM?.md")))
 --more--
```

## this condition signaled from...

this function written in [sbcl/src/code/target-pathname.lisp](https://github.com/sbcl/sbcl/blob/86fa14767a792ae38e0833d5c6964233a96b959a/src/code/target-pathname.lisp#L154)

```lisp
(defun unparse-native-physical-file (pathname)
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (collect ((fragments))
      (cond
        ((pathname-component-present-p name)
         (unless (stringp name)         ; some kind of wild field
           (no-native-namestring-error
            pathname "of the ~S component ~S." :name name))
         (fragments name)
         (when (pathname-component-present-p type)
           (unless (stringp type)       ; some kind of wild field
             (no-native-namestring-error
              pathname "of the ~S component ~S" :type type))
           (fragments ".")
           (fragments type)))
        ((pathname-component-present-p type) ; type without a name
         (no-native-namestring-error
          pathname
          "there is a ~S component but no ~S component" :type :name)))
      (apply #'concatenate 'simple-string (fragments)))))
```

So it should not be used that namestrings has wildcard in terms of UNIX. It simply signals `SB-IMPL:NO-NATIVE-NAMESTRING-ERROR`.
