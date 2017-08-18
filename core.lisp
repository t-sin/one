(in-package :cl-user)
(defpackage one/core
  (:use :cl)
  (:export :$scan
           :$call-if
           :$gather
           :$fold))
(in-package :one/core)


(defgeneric $scan (input next-fn))
(defmethod $scan ((stream stream) (read-fn function))
  "Makes scannig behavior. It reads data from `stream` with `read-fn` and calls successor operations (`op`) until :eof."
  (lambda (op)
    (loop
       :for e := (funcall read-fn stream)
       :until (eq e :eof)
       :do (funcall op e))))

(defmethod $scan ((pathname pathname) (read-fn function))
  "Makes scanning behavior. It reads data from `pathname` with `read-fn` and calls successor operations (`op`) until :eof."
  (lambda (op)
    (with-open-file (in pathname
                        :direction :input)
      (funcall ($scan in read-fn) op))))

(defmethod $scan ((sequence sequence) (step-fn function))
  "Makes scanning behavior. It calls successor operations (`op`) on contents of `sequence`."
  (cond ((listp sequence)
         (lambda (op)
           (loop
              :for e :in sequence :by step-fn
              :do (funcall op e))))
        ((vectorp sequence)
         (lambda (op)
           (loop
              :for e :across sequence
              :do (funcall op e))))))

(defun $call-if (predicate next-op)
  "Makes selective operation. The operation made by `$call-if` calls successor operations (`next-op`) when `predicate` returns true."
  (lambda (input)
    (when (funcall predicate input)
      (funcall next-op input))))

(defun $gather (gather-op)
  "Makes gathering operation. The operation made by `$gather` returns two functions:

1. buffering function locally named `slurp`
2. dumping function locally named `barf`

'Gathering' means it buffers values which is applied with `slurp`. `barf` dumps buffrred values as list with applying `gather-op`. `$gather` may be used to traverse list-like data, for example, sorting."
  (let (buffer)
    (flet ((slurp (input) (push input buffer))
           (barf (op)
             (funcall op (funcall gather-op (nreverse buffer)))))
      (values #'slurp #'barf))))

(defun $fold (fold-op init-value)
  "Makes gathering operation. The operation made by `$fold` is similar to `$gather`, but `$fold` does not buffer all input. It can be used to `reduce` on list-like data."
  (let ((accum init-value))
    (flet ((slurp (input) (setf accum (funcall fold-op accum input)))
           (barf (op) (funcall op accum)))
      (values #'slurp #'barf))))
