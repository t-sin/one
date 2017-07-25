(in-package :cl-user)
(defpackage one
  (:use :cl))
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
    (values (lambda (input) (push input buffer))
            (lambda (op)
              (funcall op (funcall gether-op buffer))
              (setf buffer nil)))))

;;; DSL
;; ex)
;; (for #P"hoge.log" < read-line ? (search "fuga" _) > (sort _ <) . print)
;; == $ cat hoge.log | grep "fuga" | sort
(defmacro for (input op-list))
