(in-package :cl-user)
(defpackage :one
  (:use :cl)
  (:import-from :one/core
                :$scan
                :$call-if
                :$gather
                :$fold)
  (:import-from :one/io
                :read*
                :read-char*
                :read-line*
                :read-byte*
                :print*)
  (:import-from :one/error
                :one-syntax-error)
  (:export :read*
           :read-char*
           :read-line*
           :read-byte*
           :print*

           :for
           :for*))
(in-package :one)


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
      (if (member (first code) '(lambda function))
          code
          (let ((input (gensym "SLMD")))
            `(lambda (,input)
               ,(replace-place-holder input code))))
      `(function ,code)))

(defun parse (body &optional stree)
  (let ((fst (first body)))
    (cond ((null body) stree)
          ((connective-p fst)
           (if (eq fst '+>)
               (let ((connective (first body))
                     (fold-op (second body))
                     (init-value (third body)))
                 (if (connective-p init-value)
                     (parse (cddr body) (list connective stree fold-op))
                     (parse (cdddr body) (list connective stree fold-op init-value))))
               (let ((connective (first body))
                     (next-op (second body))
                     (rest (cddr body)))
                 (parse rest (list connective stree next-op)))))
          (t (error 'one-syntax-error :message
                    (format nil "Parse error: ~s is not a one's connective in ~s" fst body))))))

(defun build-scan (op optree succ-op)
  (let ((in (gensym)))
    (build optree `(lambda (,in) (funcall ($scan ,in ,op) ,succ-op)))))

(defun build-gather (op optree succ-op)
  (let ((in (gensym))
        (slurp (gensym))
        (barf (gensym)))
    `(multiple-value-bind (,slurp ,barf)
         ($gather ,op)
       (lambda (,in)
         (funcall ,(build optree slurp) ,in)
         (funcall ,barf ,succ-op)))))

(defun build-fold (op initval optree succ-op)
  (let ((in (gensym))
        (slurp (gensym))
        (barf (gensym)))
    `(multiple-value-bind (,slurp ,barf)
         ($fold ,op ,initval)
       (lambda (,in)
         (funcall ,(build optree slurp) ,in)
         (funcall ,barf ,succ-op)))))

(defun build-compose (op optree succ-op)
  (let ((in (gensym)))
    (build optree `(lambda (,in) (funcall ,succ-op (funcall ,op ,in))))))

(defun build-select (op optree succ-op)
  (let ((in (gensym)))
    (build optree `(lambda (,in) (funcall ($call-if ,op ,succ-op) ,in)))))

(defun build (stree &optional (succ-op '#'identity))
  (cond ((null stree) succ-op)
        ((= (length stree) 3)
         (destructuring-bind (connective optree op)
             stree
           (setf op (simplified-lambda op))
           (ecase connective
             (< (build-scan op optree succ-op))
             (> (build-gather op optree succ-op))
             (+> (build-fold op nil optree succ-op))
             ($ (build-compose op optree succ-op))
             (? (build-select op optree succ-op)))))
        ((= (length stree) 4)
         (destructuring-bind (connective optree op init-value)
             stree
           (setf op (simplified-lambda op))
           (ecase connective
             (+> (build-fold op init-value optree succ-op)))))))

(defmacro for (input &body body)
  (if (and (symbolp input) (string= (symbol-name input) "-"))
      `(funcall ,(build (parse (replace-connective body))) *standard-input*)
      `(funcall ,(build (parse (replace-connective body))) ,input)))

(defmacro for* (input &body body)
  `(one:for ,input ,@(append body '($ one:print*))))
