(defpackage :trivial-sbcl-pathname/test.basic
  (:use :cl :rove))
(in-package :trivial-sbcl-pathname/test.basic)

(defun length-of-escaped-is-2 (ch)
  (let* ((escaped (trivial-sbcl-pathname::glob-char ch))
         (len (length escaped)))
    (ok (= len 2)
        (format nil "a char ~s is espcaped as ~s, its length should be 2"
                ch escaped))))

(deftest +glob-chars+
  (length-of-escaped-is-2 #\?)
  (length-of-escaped-is-2 #\*)
  (length-of-escaped-is-2 #\[))

(defun is-escaped (input expected)
  (let ((actual (trivial-sbcl-pathname:escape-glob input)))
    (ok (string= actual expected)
        (format nil "~s is escaped:~%~tactual: ~s~%~texpected: ~s"
                input expected actual))))

(deftest escape-glob
  (testing "escaped charactors: '?', '*', '['"
    (is-escaped "foobar2000.exe" "foobar2000.exe")
    (is-escaped "IMG_0042.jp*g" "IMG_0042.jp\\*g")
    (is-escaped "*.jpg" "\\*.jpg")
    (is-escaped "src/pages/articles/[id]/index.tsx" "src/pages/articles/\\[id]/index.tsx"))
  (testing "not escaped caractors: '{'"
    ;(is-escaped "{hoge,fuga,piyo}.jpg" "\\{hoge,fuga,piyo}.jpg")))
    (is-escaped "{hoge,fuga,piyo}.jpg" "{hoge,fuga,piyo}.jpg")))
