(defpackage :sb-pathname
  (:nicknames :sb-path)
  (:use :cl)
  (:export :escape-glob))
(in-package :sb-pathname)

(defparameter +glob-chars+
  '((#\? . #.(format nil "~a~a" #\\ #\?))
    (#\* . #.(format nil "~a~a" #\\ #\*))
    (#\[ . #.(format nil "~a~a" #\\ #\[))))

(defun glob-char (ch)
  (cdr (assoc ch +glob-chars+ :test #'char=)))

(defun glob-char-p (ch)
  (not (null (glob-char ch))))

;; TODO: it's a temporal name.
;; I don't like this name and if escaping behaviors are different on other platform, this name is not suitable.
(defun escape-glob (str)
  (with-output-to-string (stream)
    (loop
      :for ch :across str
      :for escaped := (glob-char ch)
      :do (if (null escaped)
              (write-char ch stream)
              (write-string escaped stream)))))
