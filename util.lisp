(in-package :cl-user)
(defpackage :one/util
  (:use :cl)
  (:export :read*
           :read-char*
           :read-line*
           :print*))
(in-package :one/util)


(defun read* (stream)
  (read stream nil :eof))

(defun read-char* (stream)
  (read-char stream nil :eof))

(defun read-line* (stream)
  (read-line stream nil :eof))

(defun print* (input)
  (format t "~a~%" input))
