(defpackage :sb-pathname/test.filesystem
  (:use :cl :rove))
(in-package :sb-pathname/test.filesystem)

(defun try-probe-file (p)
  (handler-case
      (progn
        (probe-file p)
        t)
    (t (s)
      (format *error-output* "condition ~a is signaled ~%details: ~a" s (describe s))
      nil)))

(defun can-probe-file (pathname)
  (let* ((rawpath (format nil "test/files/~a" pathname))
         (escaped (sb-path:to-sb-pathname rawpath)))
    (ok (try-probe-file escaped)
        (format nil "open an existing file~%~tpathname: ~s~%~tescaped as ~s" rawpath escaped))))

(deftest probe-file-with-escaped-pathname
  #-windows
  (testing "excluding windows"
    (can-probe-file "01_question-mark_?.txt")
    (can-probe-file "02_asterisk_*.txt"))
  (testing "any platforms"
    (can-probe-file "03_square-braket_open_[.txt")
    (can-probe-file "04_square-braket_closed_}.txt")
    (can-probe-file "05_curly-bracket_open_{.txt")
    (can-probe-file "06_curly-bracket_closed_}.txt")))

(defun can-probe-file-as-system-relative-pathname (pathname)
  (let* ((rawpath (format nil "test/files/~a" pathname))
         (escaped (sb-path:to-sb-pathname rawpath))
         ;; NOTE: THE TIMING of applying to-sb-pathname is VERY IMPORTANT.
         ;;
         ;; the point of the issue of this library are **pathname processes has done by SBCL**
         ;; so any pathname function (including ASDF's and UIOP's one) may call CL-standard pathname functions.
         ;;
         ;; in this test, calling to-sb-pathname before asdf:system-relative-pathname is OK.
         ;; calling asdf one before to-sb-pathname is NG because of calling CL pathname function internally.
         (system-relative (asdf:system-relative-pathname :sb-pathname escaped)))
    (ok (try-probe-file system-relative)
        (format nil "open an existing file~%~tpathname: ~s~%~tescaped as ~s" rawpath escaped))))

(deftest probe-file-with-escaped-pathname-with-asdf-pathname-function
  #-windows
  (testing "excluding windows"
    (can-probe-file-as-system-relative-pathname "01_question-mark_?.txt")
    (can-probe-file-as-system-relative-pathname "02_asterisk_*.txt"))
  (testing "any platforms"
    (can-probe-file-as-system-relative-pathname "03_square-braket_open_[.txt")
    (can-probe-file-as-system-relative-pathname "04_square-braket_closed_}.txt")
    (can-probe-file-as-system-relative-pathname "05_curly-bracket_open_{.txt")
    (can-probe-file-as-system-relative-pathname "06_curly-bracket_closed_}.txt")))
