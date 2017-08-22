(in-package :cl-user)
(defpackage :one/error
  (:use :cl)
  (:export :one-error
           :one-syntax-error))
(in-package :one/error)


(define-condition one-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "One error.~%~a~%" (message condition)))))

(define-condition one-syntax-error (one-error) ())
