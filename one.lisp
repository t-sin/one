(in-package :cl-user)
(defpackage one
  (:use :cl)
  (:import-from :one/io
                :read*
                :read-char*
                :read-line*
                :print*)
  (:export :read*
           :read-char*
           :read-line*
           :print*

           :for
           :for*))
(in-package :one)


;;; processing

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

;;; DSL

(defun connective-p (e)
  (member e '(< > +> $ ?)
          :test (lambda (a b)
                  (and (symbolp a) (string= (symbol-name a) (symbol-name b))))))

(defun replace-connective (body)
  (loop
     :for e :in body
     :collect (if (connective-p e)
                  (intern (symbol-name e) :one)
                  e)))

(defun place-holder-p (e)
  (and (symbolp e) (string= (symbol-name e) "_")))

(defun count-place-holder (code)
  (count '_ code
         :key #'place-holder-p
         :test #'string=))

(defun replace-place-holder (var code)
  (loop
     :for e :in code
     :collect (if (place-holder-p e)
                  var
                  (if (listp e)
                      (replace-place-holder var e)
                      e))))

(defun simplified-lambda (code)
  (if (listp code)
    (let ((input (gensym)))
      `(lambda (,input)
           ,(replace-place-holder input code)))
    `(function ,code)))

(defun parse (body &optional stree)
  (let ((fst (first body)))
    (cond ((null body) stree)
          ((connective-p fst)
           (if (eq fst '+>)
               (let ((connective (first body))
                     (fold-op (second body))
                     (init-value (third body)))
                 (print (connective-p init-value))
                 (if (connective-p init-value)
                     (parse (cddr body) (list connective stree fold-op))
                     (parse (cdddr body) (list connective stree fold-op init-value))))
               (let ((connective (first body))
                     (next-op (second body))
                     (rest (cddr body)))
                 (parse rest (list connective stree next-op)))))
          (t (error (format nil "invalid syntax: ~s" body))))))

(defun build (stree &optional (ops #'identity))
  (cond ((null stree) ops)
        (t (destructuring-bind (connective optree next-op)
               stree
             (setf next-op (simplified-lambda next-op))
             (ecase connective
               ;; scanning behavior
               (< (let ((in (gensym)))
                    (build optree `(lambda (,in) (funcall ($scan ,in ,next-op) ,ops)))))
               ;; gathering behavior
               (> (let ((in (gensym))
                        (slurp (gensym))
                        (barf (gensym)))
                    `(multiple-value-bind (,slurp ,barf)
                         ($gather ,next-op)
                       (lambda (,in) (funcall ,(build optree slurp) ,in)
                               (funcall ,barf ,ops)))))
               ;; folding behavior
               ;; conmposing behavior
               ($ (let ((in (gensym)))
                    (build optree `(lambda (,in) (funcall ,ops (funcall ,next-op ,in))))))
               ;; selectiver behavior
               (? (let ((in (gensym)))
                    (build optree `(lambda (,in) (funcall ($call-if ,next-op ,ops) ,in))))))))))


(defmacro for (input &body body)
  (if (and (symbolp input) (string= (symbol-name input) "-"))
      `(funcall ,(build (parse (replace-connective body))) ,*standard-input*)
      `(funcall ,(build (parse (replace-connective body))) ,input)))

(defmacro for* (input &body body)
  `(one:for ,input ,@(append body '($ one:print*))))
