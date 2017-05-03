(in-package :cl-user)
(defpackage one
  (:nicknames :o)
  (:use :cl))
(in-package :one)

#|
CL-USER> (funcall (chain #'id #'$print) 3)
;=> 3
3
|#
(defun chain (&rest chain-operators)
  "Chain operators serially. Operators specified must be defined with `define-chain-op`."
  (loop
     :for (op . rest) :on (reverse chain-operators)
     :for chained-op := (funcall op #'identity) :then (funcall op chained-op)
     :finally (return-from chain chained-op)))

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

;;; add-n: parametric chain operator
;;(defun add-n (num n) nil)

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

;;; I want to bring split-sequence into *one* in future, but now it's experiment...
#|
CL-USER> (funcall (/split-comma (chain #'reads #'add1 #'$print)) "41,42,43")
;=>42
;=>43
;=>44
(42 43 44)
|#
(defun /split-comma (chained-op)
  (lambda (string)
    (loop :named /split
       :for delim-pos := (position #\, string :start right)
       :with len := (length string)
       :with right := 0
       :while (< right len)
       :collect (if (null delim-pos)
                    (prog1
                      (funcall chained-op (subseq string right))
                      (setf right len))
                    (prog1
                      (funcall chained-op (subseq string right delim-pos))
                      (setf right (1+ delim-pos)))))))
