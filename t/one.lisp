(in-package :cl-user)
(defpackage one-test
  (:use :cl
        :one
        :prove))
(in-package :one-test)

;; NOTE: To run this test file, execute `(asdf:test-system :one)' in your Lisp.

(plan 7)

(defun testdat (path)
  (merge-pathnames path *load-pathname*))


(subtest "body-form --- loop body"
  (is (one::body-form 'line nil)
      '(one::collect line))
  (is (one::body-form 'line '(1 2 3 4))
      '(do (progn 1 2 3 4))))

(subtest "loop-form --- whole loop"
  (is (one::loop-form 'line 'in 'reader nil)
      '(loop for line = (reader in nil :eof)
          one::until (eq line :eof)
          one::collect line))
  (is (one::loop-form 'line 'in 'reader '(body1 body2))
      '(loop for line = (reader in nil :eof)
          one::until (eq line :eof)
          one::do (progn body1 body2))))

(subtest "with-input-from-file"
  (is-expand (one::with-input-from-file (var "path") body)
             (with-open-file (var "path"
                              :direction :input
                              :element-type 'character)
               body)))

(subtest "for-form --- whole form without reader functinon"

  (let ((prove.test::*gensym-alist*))
    (is (one::for-form 'line "path" 'read 'nil)
        '(one::with-input-from-file ($fin "path")
          (loop for line = (read $fin nil :eof)
             one::until (eq line :eof)
             one::collect line))
        :test #'prove.test::gensym-tree-equal))

  (let ((prove.test::*gensym-alist*))
    (is (one::for-form 'line #P"path" 'read-line nil)
        '(one::with-input-from-file ($fin #P"path")
          (loop for line = (read-line $fin nil :eof)
             one::until (eq line :eof)
             one::collect line))
        :test #'prove.test::gensym-tree-equal))

  (let ((prove.test::*gensym-alist*))
    (is (one::for-form 'line #P"path" 'read-line '(body1 body2))
        '(one::with-input-from-file ($fin #P"path")
          (loop for line = (read-line $fin nil :eof)
             one::until (eq line :eof)
             one::do (progn body1 body2)))
        :test #'prove.test::gensym-tree-equal))

  (is (one::for-form 'line :stdin 'read-char '(body1 body2))
       '(loop for line = (read-char one::*standard-input* nil :eof)
           one::until (eq line :eof)
           one::do (progn body1 body2)))

   (subtest "return nil when in is not one of string, pathname and :stdin"
            (is (one::for-form 'l 42 'read-line '(body))
                nil)))


(subtest "for (using read)"
  (subtest "with no body"
    (is (one:for (l (testdat "nums.txt")))
        '(1 3 5 7))
    (is (one:for (l (testdat "strs.txt")))
        '("the" "quick" "brown" "fox"
          "jumps" "over"
          "the" "red" "lazy" "dog")))

  (subtest "with body"
    (let ((n 0))
      (is (one:for (l (testdat "nums.txt"))
            (incf n l))
          nil)
      (is n (reduce #'+ '(1 3 5 7))))
    (let ((s ""))
      (is (one:for (l (testdat "strs.txt"))
            (setf s (format nil "~a ~a" s l)))
          nil)
      (is s " the quick brown fox jumps over the red lazy dog"))))

(subtest "forl (for line)"
  (subtest "with no body"
    (is (one:forl (l (testdat "nums.txt")))
        '("1" "3" "5" "7"))

    (is (one:forl (l (testdat "strs.txt")))
        '("the quick"
          "brown fox"
          "jumps over"
          "the red"
          "lazy dog")))

  (subtest "with body"
    (let ((s "1
3
5
7"))
      (is (one:forl (l (testdat "strs.txt"))
            (setf s (format nil "~a ~a" s l)))
          nil)
      (is s " 1 3 5 7"))
    (let ((s ""))
      (is (one:forl (l (testdat "strs.txt"))
            (setf s (format nil "~a ~a" s l)))
          nil)
      (is s " the quick brown fox jumps over the red lazy dog"))))

(defmacro with-stdin ((str) &body body)
  (let ((in (gensym)))
    `(with-input-from-string (,in ,str)
       (let ((*standard-input* ,in))
         ,@body))))

(subtest "interacting to *standard-input*"
  (with-stdin ("1 2 3")
    (is (one:for (l one:stdin))
        '(1 2 3))
    (is (one:forl (l one:stdin))
        '("1 2 3"))))


(finalize)
