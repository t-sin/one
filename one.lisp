(in-package :cl-user)
(defpackage one
  (:nicknames :o)
  (:use :cl))
(in-package :one)

;;;; this is a rough sketch for `one`

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


(defstruct pipe
  push pop close set-callback)

(defun make-pipe ()
  (let ((buffer)
        (head)
        (callback-fn))
    (flet ((push-obj (obj)
             (when (null head)
               (setf head obj))
             (setf buffer `(,@buffer ,obj))
             (when callback-fn
               (funcall callback-fn head))
             head)
           (pop-obj ()
             (if buffer
                 (let ((previous-head head))
                   (setf head (cadr buffer)
                         buffer (cdr buffer))
                   (values previous-head t))
                 (values nil nil)))
           (close-pipe ()
             (setf head :closed
                   buffer :closed))
           (set-callback (fn)
             (when (functionp fn)
               (setf callback-fn fn))))
      (make-pipe :push #'push-obj
                 :pop #'pop-obj
                 :close #'close-pipe
                 :set-callback #'set-callback))))

(defmacro with-object-pipe ()
  nil)

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





#|
- can use standard Common Lisp operators/functions
- can use other library functions (e.g. split-sequence)
- value-to-value operations are lazy evaluated
    - distinguish a kind of *one operators* by '<', '>'

(one:for "1,2,3,4,5" (<split #\,) read (+ 1) (>remove-if #'oddp) >+)
=> (let* ((input "1,2,3,4,5")
          (pipe1 (make-pipe-from-sequense (split-sequence #\, input)))
          (pipe2 (transform-pipe pipe1 (lambda (x) (+ 1 (lambda (x) (read x))))))
          (pipe3 (make-pipe pipe2 (remove-if #'oddp {})))
          (pipe4 (make-pipe pipe3 (+ @{}))))
      (dump-pipe pipe4))
; => 8
|#

#|
(one:for <input> <transform...>)
transforms:
- symbol ... 1 args function
- list ... muiti-args function (but passed one args; input)
  - simple list ... args placed at last
  - list has _ symbol ... args placed at position of _
- readers ... function has special behavior; result stores in object pipe
  - $<(fun args... _) ... multi time firing of successor
  - $>(fun args... _) ... outputs maybe fewer than input

(one:for1 <input> <transform>)
|#


