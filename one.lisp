(in-package :cl-user)
(defpackage one
  (:use :cl)
  (:export :for
           :forl))
(in-package :one)


(defun body-form (var body)
  (if body
      `(do (progn ,@body))
      `(collect ,var)))

(defun loop-form (var input-stream read-fn body)
  `(loop for ,var = (,read-fn ,input-stream nil :eof)
      until (eq ,var :eof)
      ,@(body-form var body)))

(defmacro with-input-from-file ((var path) &body body)
  `(with-open-file (,var ,path
                    :direction :input
                    :element-type 'character) ; use :inquisitor
     ,@body))

(defun for-form (var in reader body)
  (cond ((or (typep in 'string)
             (typep in 'pathname))
         (let ((fin (gensym)))
           `(with-input-from-file (,fin ,in)
              ,(loop-form var fin reader body))))
        ((eq in :stdin)
         (loop-form var '*standard-input* reader body))))


(defmacro for ((var in) &body body)
  (for-form var in 'read body))


(defmacro forl ((var in) &body body)
  (for-form var in 'read-line body))
