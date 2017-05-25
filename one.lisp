(in-package :cl-user)
(defpackage one
  (:nicknames :o)
  (:use :cl))
(in-package :one)

#|
CL-USER> (funcall (transform #'id #'$print) 3)
;=> 3
3
|#
(defun transform (&rest transform-operators)
  "Transform operators serially. Operators specified must be defined with `define-transform-op`."
  (loop
     :for (op . rest) :on (reverse transform-operators)
     :for transformed-op := (funcall op #'identity) :then (funcall op transformed-op)
     :finally (return-from transform transformed-op)))

(defmacro define-transform-op (fn-name (input-var) &body body)
  "Define transform-operator. Transform-operators is a function that returns single result.
Or transform-operators is a function that is a transform."
  (let ((transformed-op (gensym)))
    `(defun ,fn-name (,transformed-op)
       (lambda (,input-var)
         (funcall ,transformed-op (progn ,@body))))))

;;; transform-operators
;;; they are constructive operators, not core.
;;; they will be moved when finished experimental
(define-transform-op id (obj)
  obj)

(define-transform-op add1 (num)
  (1+ num))

(define-transform-op reads (str)
  (read-from-string str))

(define-transform-op $print (obj)
  (print obj))

;;; add-n: parametric chain operator
;;(defun add-n (num n) nil)

;;; per or split operators?
;;; cannot define with define-op
#|
CL-USER> (with-input-from-string (in (format nil "42~%43~%44"))
           (transform in #'/line #'reads #'add1))
(43 44 45)
|#
(defun /line (transformed-op)
  (lambda (stream)
    (loop
       :for line := (read-line stream nil nil)
       :while line
       :collect (funcall transformed-op line)))) ;; bad performance in future

;;; I want to bring split-sequence into *one* in future, but now it's experiment...
#|
CL-USER> (funcall (/split-comma (transform #'reads #'add1 #'$print)) "41,42,43")
;=>42
;=>43
;=>44
(42 43 44)
|#
#|
CL-USER> (with-input-from-string (in (format nil "1,2~%3,4~%42"))
           (funcall (transform #'/line #'/split-comma #'reads #'add1 #'$print) in))
2
3
4
5
43
((2 3) (4 5) (43))
|#
(defun /split-comma (transformed-op)
  (lambda (string)
    (loop :named /split
       :for delim-pos := (position #\, string :start right)
       :with len := (length string)
       :with right := 0
       :while (< right len)
       :collect (if (null delim-pos)
                    (prog1
                      (funcall transformed-op (subseq string right))
                      (setf right len))
                    (prog1
                      (funcall transformed-op (subseq string right delim-pos))
                      (setf right (1+ delim-pos)))))))


(defun make-object-stream ()
  (let ((stream)
        (head))
    (labels ((push-object (obj)
               (when (null head)
                 (setf head obj))
               (setf stream `(,@stream ,obj))
               head)
             (pop-object ()
               (if stream
                   (prog1 (values head nil)
                       (setf head (cadr stream)
                             stream (cdr stream)))
                   (values nil t)))
             (clear-stream ()
               (setf head nil
                     stream nil)))
      (values #'push-object #'pop-object #'clear-stream))))

#|
CL-USER> (multiple-value-bind (pushobj popobj h)
             (make-object-stream)
           (funcall (funcall #'>oddp pushobj) (funcall (transform #'/split-comma #'reads) "1,2,3,4,5"))
           (loop
              :for (v nil-p) := (multiple-value-list (funcall popobj))
              :until nil-p
              :collect v))
(1 3 5)
|#
(defun >oddp (push-fn)
  (lambda (numbers)
    (loop
       :for n :in numbers
       :when (oddp n)
       :do (funcall push-fn n))))

