(defpackage :sb-pathname
  (:nicknames :sb-path)
  (:use :cl)
  (:export :to-sb-pathname
           :from-sb-pathname))
(in-package :sb-pathname)

(defparameter +escape-char+
  #+windows #\^
  #-windows #\\)
(defparameter +escape-target-chars+
  #+windows '(#\[)
  #-windows '(#\? #\* #\[))

(defun %escape (ch)
  (format nil "~a~a" +escape-char+ ch))

(defparameter +escape-mapping+
  (loop
    :for ch :in +escape-target-chars+
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

(defun escape-sequence-p (ch1 ch2)
  (and (typep ch1 'character)
       (char= ch1 +escape-char+)
       (member ch2 +escape-target-chars+ :test #'char=)))

(defun from-sb-pathname (pathname)
  (flet ((is-escape-char (ch)
           (char= ch +escape-char+))
         (is-escape-target (ch)
           (member ch +escape-target-chars+ :test #'char=)))
    (with-output-to-string (stream)
      (loop
        :for ch :across pathname
        :with after-escape-char-p := nil
        :do (if after-escape-char-p
                (progn
                  (unless (is-escape-target ch)
                    (write-char +escape-char+ stream))
                  (unless (is-escape-char ch)
                    (write-char ch stream)))
                (unless (is-escape-char ch)
                  (write-char ch stream)))
        :do (setf after-escape-char-p (is-escape-char ch))))))
