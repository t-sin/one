(in-package :cl-user)
(defpackage one
  (:use :cl)
  (:export :read*
           :read-char*
           :read-line*
           :read-byte*
           :print*

           :for))
(in-package :one)


;;; IO

(defun read* (stream)
  (read stream nil :eof))

(defun read-char* (stream)
  (read-char stream nil :eof))

(defun read-line* (stream)
  (read-line stream nil :eof))

(defun read-byte* (stream)
  (read-byte stream nil :eof))

(defun print* (input)
  (format t "~a~%" input))


;;; processing

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
      (funcall ($scan in read-fn) op))))

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

(defun $call-if (predicate next-op)
  (lambda (input)
    (when (funcall predicate input)
      (funcall next-op input))))

(defun $map (next-op)
  (lambda (sequence)
    (mapcar next-op (coerce sequence 'list))))

(defun $gather (gather-op)
  (let (buffer)
    (flet ((slurp (input) (push input buffer))
           (barf (op)
             (funcall op (funcall gather-op (nreverse buffer)))))
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
          ((connective-p fst) (let ((connective (first body))
                                    (next-op (second body))
                                    (rest (cddr body)))
                                (parse rest (list connective stree next-op))))
          (t (error (format nil "invalid syntax: ~s" body))))))

(defun build (stree &optional (ops #'identity))
  (cond ((null stree) ops)
        (t (destructuring-bind (connective optree next-op)
               stree
             (setf next-op (simplified-lambda next-op))
             (ecase connective
               (< (let ((in (gensym)))
                    (build optree `(lambda (,in) (funcall ($scan ,in ,next-op) ,ops)))))
               (> (let ((in (gensym))
                        (slurp (gensym))
                        (barf (gensym)))
                    `(multiple-value-bind (,slurp ,barf)
                         ($gather ,next-op)
                       (lambda (,in) (funcall ,(build optree slurp) ,in)
                               (funcall ,barf ,ops)))))
               ($ (let ((in (gensym)))
                    (build optree `(lambda (,in) (funcall ,ops (funcall ,next-op ,in))))))
               (? (let ((in (gensym)))
                    (build optree `(lambda (,in) (funcall ($call-if ,next-op ,ops) ,in))))))))))


(defmacro for (input &body body)
  (if (and (symbolp input) (string= (symbol-name input) "-"))
      `(funcall ,(build (parse (replace-connective body))) ,*standard-input*)
      `(funcall ,(build (parse (replace-connective body))) ,input)))
