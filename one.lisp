(in-package :cl-user)
(defpackage one
  (:use :cl)
  (:export :for
           :forl))
(in-package :one)



(defmacro with-input-from-file ((var path) &body body)
  `(with-open-file (,var ,path
                    :direction :input
                    :element-type 'character) ; use :inquisitor
     ,@body))



(defmacro for ((var in) &body body)
  (for-form var in 'read body))


(defmacro forl ((var in) &body body)
  (for-form var in 'read-line body))
