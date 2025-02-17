(defpackage :sb-pathname/test.basic
  (:use :cl :rove))
(in-package :sb-pathname/test.basic)

(defun char-escaped-as (ch expected)
  (let* ((escaped (sb-path::escape-char ch)))
    (ok (string= escaped expected)
        (format nil "a char ~s is escaped as ~s, but expected ~s"
                ch escaped expected))))

#+windows
(deftest escape-char/windows
  (char-escaped-as #\[ "^["))

#-windows
(deftest escape-char/unix
  (char-escaped-as #\? "\\?")
  (char-escaped-as #\* "\\*")
  (char-escaped-as #\[ "\\["))

(defun is-escaped-correctly (input expected)
  (let ((actual (sb-path:to-sb-pathname input)))
    (ok (string= actual expected)
        (format nil "~s is escaped:~%~tactual:   ~s~%~texpected: ~s"
                input expected actual))))

#+windows
(deftest to-sb-pathname/windows
  (testing "escaped charactors: '['"
    (is-escaped-correctly "foobar2000.exe" "foobar2000.exe")
    (is-escaped-correctly "src/pages/articles/[id]/index.tsx" "src/pages/articles/^[id]/index.tsx"))
  (testing "not escaped caractors: '{'"
    (is-escaped-correctly "{hoge,fuga,piyo}.jpg" "{hoge,fuga,piyo}.jpg")))

#-windows
(deftest to-sb-pathname/unix
  (testing "escaped charactors: '?', '*', '['"
    (is-escaped-correctly "foobar2000.exe" "foobar2000.exe")
    (is-escaped-correctly "IMG_0042.jp?g" "IMG_0042.jp\\?g")
    (is-escaped-correctly "*.jpg" "\\*.jpg")
    (is-escaped-correctly "src/pages/articles/[id]/index.tsx" "src/pages/articles/\\[id]/index.tsx"))
  (testing "not escaped caractors: '{'"
    (is-escaped-correctly "{hoge,fuga,piyo}.jpg" "{hoge,fuga,piyo}.jpg")))

(defun is-unescaped (input expected)
  (let ((actual (sb-path:from-sb-pathname input)))
    (ok (string= actual expected)
        (format nil "~s is unescaped~%~tactual:   ~s~%~texpected: ~s"
                input actual expected))))

#+windows
(deftest from-sb-pathname/windows
  (testing "pathname has no escape sequences"
    (is-unescaped "foobar2000.exe" "foobar2000.exe")
    (is-unescaped ".bash_profile" ".bash_profile")
    (is-unescaped "C:\\Program Files\\Abc Soft\\Abc.exe" "C:\\Program Files\\Abc Soft\\Abc.exe"))
  (testing "escaped charactors: '?', '*', '['"
    (is-unescaped "src/pages/articles/^[id]/index.tsx" "src/pages/articles/[id]/index.tsx"))
  (testing "not escaped caractors: '{'"
    (is-escaped-correctly "{hoge,fuga,piyo}.jpg" "{hoge,fuga,piyo}.jpg")))

#-windows
(deftest from-sb-pathname/unix
  (testing "pathname has no escape sequences"
    (is-unescaped "foobar2000.exe" "foobar2000.exe")
    (is-unescaped ".bash_profile" ".bash_profile")
    (is-unescaped "C:\\Program Files\\Abc Soft\\Abc.exe" "C:\\Program Files\\Abc Soft\\Abc.exe"))
  (testing "escaped charactors: '?', '*', '['"
    (is-unescaped "IMG_0042.jp\\?g" "IMG_0042.jp?g")
    (is-unescaped "\\*.jpg" "*.jpg")
    (is-unescaped "src/pages/articles/\\[id]/index.tsx" "src/pages/articles/[id]/index.tsx"))
  (testing "not escaped caractors: '{'"
    (is-escaped-correctly "{hoge,fuga,piyo}.jpg" "{hoge,fuga,piyo}.jpg")))

(defun identical-by-two-conversion (input)
  (let* ((escaped (sb-path:to-sb-pathname input))
         (unescaped (sb-path:from-sb-pathname escaped)))
    (ok (string= unescaped input)
        (format nil "~s is escaped as ~s,~%~tand unescaped: ~s"
                input escaped unescaped))))

(deftest conversion-identity
  (testing "no escaped"
    (identical-by-two-conversion "foobar2000.exe")
    (identical-by-two-conversion ".bash_profile"))
  (testing "special characters on Windows"
    (identical-by-two-conversion "C:\\Program Files\\Abc Soft\\Abc.exe"))
  (testing "escaped on Windows"
    (identical-by-two-conversion "src/pages/articles/[id]/index.tsx"))
  (testing "escaped on UNIX"
    (identical-by-two-conversion "IMG_0042.jp?g")
    (identical-by-two-conversion "*.jpg")
    (identical-by-two-conversion "src/pages/articles/[id]/index.tsx")))
