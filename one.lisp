(in-package :cl-user)
(defpackage one
  (:nicknames :o)
  (:use :cl))
(in-package :one)

#|
CL-USER>  (chain 3 #'id #'$print)
;=> 3
3
|#
(defun chain (input &rest chain-operators)
  "Chain operators serially. Operators specified must be defined with `define-chain-op`."
  (loop
     :for (op . rest) :on (reverse chain-operators)
     :for chained-op := (funcall op #'identity) :then (funcall op chained-op)
     :finally (return-from chain (funcall chained-op input))))

(defmacro define-chain-op (fn-name (input-var) &body body)
  "Define chain-operator. Chain-operators is a function that returns single result.
Or chain-operators is a function that is a transform."
  (let ((chained-op (gensym)))
    `(defun ,fn-name (,chained-op)
       (lambda (,input-var)
         (funcall ,chained-op (progn ,@body))))))

;;; chain-operators
;;; they are constructive operators, not core.
;;; they will be moved when finished experimental
(define-chain-op id (obj)
  obj)

(define-chain-op add1 (num)
  (1+ num))

(define-chain-op reads (str)
  (read-from-string str))

(define-chain-op $print (obj)
  (print obj))


;;; per or split operators?
;;; cannot define with define-op
#|
CL-USER> (with-input-from-string (in (format nil "42~%43~%44"))
           (chain in #'/line #'reads #'add1))
(43 44 45)
|#
(defun /line (chained-op)
  (lambda (stream)
    (loop
       :for line := (read-line stream nil nil)
       :while line
       :collect (funcall chained-op line)))) ;; bad performance in future
