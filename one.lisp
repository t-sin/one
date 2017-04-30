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

;;; they are constructive operators, not core.
;;; they will be moved when finished experimental
(defun id (chained-op)
  (lambda (input)
    (funcall chained-op input)))

(defun add1 (chained-op)
  (lambda (input)
    (1+ (funcall chained-op input))))

(defun reads (chained-op)
  (lambda (string)
    (read-from-string (funcall chained-op string))))

(defun $print (chained-op)
  (lambda (input)
    (print (funcall chained-op input))))

(defun /line (next-op)
  (lambda (stream)
    (loop
       :for line := (read-line stream nil nil)
       :while line
       :collect (funcall next-op line))))

