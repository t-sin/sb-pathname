(defpackage :sb-pathname
  (:nicknames :sb-path)
  (:use :cl)
  (:export :to-sb-pathname
           :from-sb-pathname))
(in-package :sb-pathname)

(defparameter +escape-char-windows+ #\^)
(defparameter +escape-target-chars-windows+ '(#\[))

(defparameter +escape-char-unix+ #\\)
(defparameter +escape-target-chars-unix+ '(#\? #\* #\[))

(defun %escape (ch)
  (format nil "~a~a"
          #+windows +escape-char-windows+
          #-windows +escape-char-unix+
          ch))

(defparameter +escape-mapping+
  (loop
    :for ch :in
       #+windows +escape-target-chars-windows+
       #-windows +escape-target-chars-unix+
    :collect (cons ch (%escape ch))))

(defun escape-char (ch)
  (cdr (assoc ch +escape-mapping+ :test #'char=)))

(defun to-sb-pathname (pathname)
  (with-output-to-string (stream)
    (loop
      :for ch :across pathname
      :for escaped := (escape-char ch)
      :do (if (null escaped)
              (write-char ch stream)
              (write-string escaped stream)))))

(defun from-sb-pathname (pathname))
