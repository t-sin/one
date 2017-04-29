(in-package :cl-user)
(defpackage one
  (:nicknames :o)
  (:use :cl))
(in-package :one)

(defun id (chained-op)
  (lambda (input)
    (funcall chained-op input)))

(defun $print (chained-op)
  (lambda (input)
    (print (funcall chained-op input))))

(defun /line (next-op)
  (lambda (input)
    (loop
       :for line := (read-line input nil nil)
       :while line
       :do (funcall next-op line))))

#|
CL-USER>  (chain 3 #'id #'$print #'identity)
;=> 3
3
|#
(defun chain (input &rest operators)
  (loop
     :for op :in operators
     :for chained-op := op :then (funcall chained-op op)
     :finally (return-from chain (funcall chained-op input))))
