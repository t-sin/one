(in-package :cl-user)
(defpackage :one/io
  (:use :cl)
  (:export :read*
           :read-char*
           :read-line*
           :print*))
(in-package :one/io)


(defun read* (stream)
  (read stream nil :eof))

(defun read-char* (stream)
  (read-char stream nil :eof))

(defun read-line* (stream)
  (read-line stream nil :eof))

(defun print* (input)
  (format t "~a~%" input))
