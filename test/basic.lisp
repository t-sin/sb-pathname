(defpackage :sb-pathname/test.basic
  (:use :cl :rove))
(in-package :sb-pathname/test.basic)

(defun length-of-escaped-is-2 (ch)
  (let* ((escaped (sb-path::escape-char ch))
         (len (length escaped)))
    (ok (= len 2)
        (format nil "a char ~s is espcaped as ~s, its length should be 2"
                ch escaped))))

(deftest escaped-string-length
  (length-of-escaped-is-2 #\?)
  (length-of-escaped-is-2 #\*)
  (length-of-escaped-is-2 #\[))

(defun is-escaped-correctly (input expected)
  (let ((actual (sb-path:to-sb-pathname input)))
    (ok (string= actual expected)
        (format nil "~s is escaped:~%~tactual: ~s~%~texpected: ~s"
                input expected actual))))

(deftest to-sb-pathname
  (testing "escaped charactors: '?', '*', '['"
    (is-escaped-correctly "foobar2000.exe" "foobar2000.exe")
    (is-escaped-correctly "IMG_0042.jp*g" "IMG_0042.jp\\*g")
    (is-escaped-correctly "*.jpg" "\\*.jpg")
    (is-escaped-correctly "src/pages/articles/[id]/index.tsx" "src/pages/articles/\\[id]/index.tsx"))
  (testing "not escaped caractors: '{'"
    ;(is-escaped-correctly "{hoge,fuga,piyo}.jpg" "\\{hoge,fuga,piyo}.jpg")))
    (is-escaped-correctly "{hoge,fuga,piyo}.jpg" "{hoge,fuga,piyo}.jpg")))
