(in-package :cl-user)
(defpackage one
  (:use :cl)
  (:export :stdin
           :for
           :forl))
(in-package :one)


(defparameter stdin *standard-input*)

(defun body-form (var body)
  (if body
      `(do (progn ,@body))
      `(collect ,var)))

(defun loop-form (var input-stream read-fn body)
  `(loop for ,var = (,read-fn ,input-stream nil :eof)
         until (eq ,var :eof)
         ,@(body-form var body)))

(defmacro with-file-input ((var in) &body body)
  (let ((fin (gensym)))
    `(with-open-file (,fin ,in
                      :direction :input
                      :element-type 'character)
       ,@body)))

(defmacro for ((var in) &body body)
  (cond ((or (typep in 'string)
             (typep in 'pathname))
         (let ((fin (gensym)))
           `(with-open-file (,fin ,in
                             :direction :input
                             :element-type 'character)
              ,(loop-form var fin 'read body))))
        ((eq in 'stdin)
         (loop-form var stdin 'read body))))
