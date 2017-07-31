(in-package :cl-user)
(defpackage one
  (:use :cl)
  (:export :read*
           :read-char*
           :read-line*
           :read-byte*

           :$scan
           :$call-if
           :$map
           :$gether
           :$compose))
(in-package :one)

;;;; this is a rough sketch for `one`

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
- list ... muiti-args function (but passed one args: input)
  - simple list ... args placed at last
  - list has _ symbol ... args placed at position of _
- readers ... function has special behavior; result stores in object pipe
  - $<(fun args... _) ... multi time firing of successor
  - $>(fun args... _) ... outputs maybe fewer than input

(one:for1 <input> <transform>)
|#


;;;; one: elementary functions

;;; input
(defun read* (stream)
  (read stream nil :eof))

(defun read-char* (stream)
  (read-char stream nil :eof))

(defun read-line* (stream)
  (read-line stream nil :eof))

(defun read-byte* (stream)
  (read-byte stream nil :eof))

(defgeneric $scan (input next-fn))
(defmethod $scan ((stream stream) (read-fn function))
  (lambda (op)
    (loop
       :for e := (funcall read-fn stream)
       :until (eq e :eof)
       :do (funcall op e))))

(defmethod $scan ((pathname pathname) (read-fn function))
  (lambda (op)
    (with-open-file (in pathname
                        :direction :input)
      (scan in read-fn))))

(defmethod $scan ((sequence sequence) (next-fn function))
  (cond ((listp sequence)
         (lambda (op)
           (loop
              :for e :in sequence
              :do (funcall op e))))
        ((vectorp sequence)
         (lambda (op)
           (loop
              :for e :across sequence
              :do (funcall op e))))))

;;; processing
;; ex)
;; - filter
;;
;; reader macro:
;;   $?(fn args...)
;;   $?fn
(defun $call-if (predicate next-op)
  (lambda (input)
    (when (funcall predicate input)
      (funcall next-op input))))

;; ex)
;; - split
(defun $map (next-op)
  (lambda (sequence)
    (mapcar next-op (coerce sequence 'list))))

;; ex)
;; - sort
;; - as list
(defun $gether (gether-op)
  (let (buffer)
    (flet ((slurp (input) (push input buffer))
           (barf (op)
             (funcall op (funcall gether-op buffer))
             (setf buffer nil)))
      (values #'slurp #'barf))))


;;; DSL
(defun connective-p (e)
  (member e '(< > $ ?)
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
                  e)))

(defun simplified-lambda (code)
  (if (listp code)
    (let ((input (gensym)))
      `(lambda (,input)
           ,(replace-place-holder input code)))
    `(function ,code)))

(defun parse (body &optional stree)
  (let ((fst (first body)))
    (cond ((null body) stree)
          ((connective-p fst) (let ((connective (first body))
                                    (next-op (second body))
                                    (rest (cddr body)))
                                (parse rest (list connective stree next-op))))
          (t (error (format nil "invalid syntax: ~s" body))))))

(defun build (stree &optional (op #'identity))
  (cond ((not (listp stree)) stree)
        ((null stree) op)
        (t (destructuring-bind (connective input next-op)
               stree
             (setf next-op (simplified-lambda next-op))
             (ecase connective
               (< (let ((input-var (gensym)))
                    `(lambda (,input-var) (funcall ($scan ,input ,next-op) ,op))))
               (> :gather)
               ($ `(funcall ,next-op ,(build input)))
               (? :call-if))))))

;; ex)
;; (for #P"hoge.log" < read-line ? (search "fuga" _) > (sort _ <) $ print)
;; == $ cat hoge.log | grep "fuga" | sort
(defmacro for (&body body))
