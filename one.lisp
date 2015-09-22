(in-package :cl-user)
(defpackage one
  (:use :cl)
  (:export :for
           :forl))
(in-package :one)


(declaim (inline read-for))
(defun read-for (input-stream read-fn body-fn)
  (let ((eof (gensym)))
    (loop for line = (funcall read-fn input-stream nil eof)
       until (eq line eof)
       collect (funcall body-fn line))))

(defmacro with-input-from-file ((var path) &body body)
  `(with-open-file (,var ,path
                    :direction :input
                    :element-type 'character) ; use :inquisitor
     ,@body))

(defun call-read-for (in read-fn &optional (body-fn #'identity))
  (cond ((or (typep in 'string)
             (typep in 'pathname))
         (with-input-from-file (fin in)
           (read-for fin read-fn body-fn)))
        ((eq in :stdin)
         (read-for *standard-input* read-fn body-fn))))


(defmacro for ((var in) &body body)
  (for-form var in 'read body))


(defmacro forl ((var in) &body body)
  (for-form var in 'read-line body))
