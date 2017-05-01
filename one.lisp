(in-package :cl-user)
(defpackage one
  (:nicknames :o)
  (:use :cl))
(in-package :one)

#|
CL-USER>  (chain 3 #'id #'$print #'identity)
;=> 3
3
|#
(defun chain (input &rest operators)
  (loop
     :for (op . rest) :on operators
     :for chained-op := (funcall op #'identity) :then (funcall op chained-op)
     :finally (return-from chain (funcall chained-op input))))

(defmacro define-op (fn-name (input-var) &body body)
  (let ((chained-op (gensym)))
    `(defun ,fn-name (,chained-op)
       (lambda (,input-var)
         (funcall ,chained-op (progn ,@body))))))

;;; they are constructive operators, not core.
;;; they will be moved when finished experimental
(define-op id (obj)
  obj)

(define-op add1 (num)
  (1+ num))

(define-op reads (str)
  (read-from-string str))

(define-op $print (obj)
  (print obj))

;; cannot define with define-op
(defun /line (chained-op)
  (lambda (stream)
    (loop
       :for line := (read-line stream nil nil)
       :while line
       :collect (funcall chained-op line)))) ;; bad performance in future
