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
           :$curry
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

(defmacro $curry (code)
  (let ((input (gensym)))
    `(lambda (,input)
       ,(replace-place-holder input code))))

(defun $compose (operators)
  (let ((op (car operators)))
    (if (null op)
        #'identity
        (lambda (input) (funcall op (funcall ($compose (cdr operators)) input))))))

;;; DSL

(defun associate-left (body)
  (labels ((symbol-equal-p (a b)
             (and (symbolp a)
                  (string= (symbol-name a) (symbol-name b))))
           (connective-p (e)
             (member e '(< > $ ?)
                     :test #'symbol-equal-p)))
    (let ((pos (position-if #'connective-p body)))
      (print pos)
      (print body)
      (if (null pos)
          body
          (if (= pos 1)
              (let ((input (nth 0 body))
                    (connective (nth pos body))
                    (rest (cddr body)))
                (list input `(,connective ,@(associate-left rest))))
              (error "invalid code: "))))))

;; ex)
;; (for #P"hoge.log" < read-line ? (search "fuga" _) > (sort _ <) $ print)
;; == $ cat hoge.log | grep "fuga" | sort
(defmacro for (&body body))
