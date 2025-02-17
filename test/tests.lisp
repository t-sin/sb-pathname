(defpackage :trivial-sbcl-pathname/test
  (:use :cl :rove))
(in-package :trivial-sbcl-pathname/test)

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

(defun try-probe-file (p)
  (handler-case
      (progn
        (probe-file p)
        t)
    (t (s)
      (format t "condition ~a is signaled ~%details: ~a" s (describe s))
      nil)))

(defun can-probe-file (pathname)
  (let* ((rawpath (format nil "test/files/~a" pathname))
         (escaped (trivial-sbcl-pathname:escape-glob rawpath)))
    (ok (try-probe-file escaped)
        (format nil "open an existing file~%~tpathname: ~s~%~tescaped as ~s" rawpath escaped))))

(deftest probe-file-with-escaped-pathname
  (can-probe-file "01_question-mark_?.txt")
  (can-probe-file "02_asterisk_*.txt")
  (can-probe-file "03_square-braket_open_[.txt")
  (can-probe-file "04_square-braket_closed_}.txt")
  (can-probe-file "05_curly-bracket_open_{.txt")
  (can-probe-file "06_curly-bracket_closed_}.txt"))

(defun can-probe-file-as-system-relative-pathname (pathname)
  (let* ((rawpath (format nil "test/files/~a" pathname))
         (escaped (trivial-sbcl-pathname:escape-glob rawpath))
         ;; NOTE: THE TIMING of applying escape-glob is VERY IMPORTANT.
         ;;
         ;; the point of the issue of this library are **pathname processes has done by SBCL**
         ;; so any pathname function (including ASDF's and UIOP's one) may call CL-standard pathname functions.
         ;;
         ;; in this test, calling escape-glob before asdf:system-relative-pathname is OK.
         ;; calling asdf one before escape-glob is NG because of calling CL pathname function internally.
         (system-relative (asdf:system-relative-pathname :trivial-sbcl-pathname escaped)))
    (ok (try-probe-file escaped)
        (format nil "open an existing file~%~tpathname: ~s~%~tescaped as ~s" rawpath escaped))))

(deftest probe-file-with-escaped-pathname-with-asdf-pathname-function
  (can-probe-file-as-system-relative-pathname "01_question-mark_?.txt")
  (can-probe-file-as-system-relative-pathname "02_asterisk_*.txt")
  (can-probe-file-as-system-relative-pathname "03_square-braket_open_[.txt")
  (can-probe-file-as-system-relative-pathname "04_square-braket_closed_}.txt")
  (can-probe-file-as-system-relative-pathname "05_curly-bracket_open_{.txt")
  (can-probe-file-as-system-relative-pathname "06_curly-bracket_closed_}.txt"))
