(in-package :cl-user)
(defpackage one-test
  (:use :cl
        :one
        :prove))
(in-package :one-test)

;; NOTE: To run this test file, execute `(asdf:test-system :one)' in your Lisp.

(plan 5)

(defun testdat (path)
  (merge-pathnames path *load-pathname*))

(subtest "for --- expansion"
   (is-expand (one:for (line #P"path"))
              (with-open-file ($in #P"path"
                               :direction :input
                               :element-type 'character)
                (loop for line = (read $in nil :eof)
                   until (eq line :eof)
                   collect line)))

   (is-expand (one:for (line one:stdin))
              (loop for line = (read cl:*standard-input* nil :eof)
                 until (eq line :eof)
                 collect line))

   (is-expand (one:for (line #P"path")
                (format t "~a~%" line))
              (with-open-file ($in #P"path"
                               :direction :input
                               :element-type 'character)
                (loop for line = (read $in nil :eof)
                   until (eq line :eof)
                   do (progn (format t "~a~%" line)))))

   (is-expand (one:for (line #P"path")
                (format t "~a~%" line)
                (format t "~a2~%" line))
              (with-open-file ($in #P"path"
                               :direction :input
                               :element-type 'character)
                (loop for line = (read $in nil :eof)
                   until (eq line :eof)
                   do (progn (format t "~a~%" line)
                             (format t "~a2~%" line))))))

(subtest "forl --- expansion"
   (is-expand (one:forl (line #P"path"))
              (with-open-file ($in #P"path"
                               :direction :input
                               :element-type 'character)
                (loop for line = (read-line $in nil :eof)
                   until (eq line :eof)
                   collect line)))

   (is-expand (one:forl (line one:stdin))
              (loop for line = (read-line cl:*standard-input* nil :eof)
                 until (eq line :eof)
                 collect line))

   (is-expand (one:forl (line #P"path")
                (format t "~a~%" line))
              (with-open-file ($in #P"path"
                               :direction :input
                               :element-type 'character)
                (loop for line = (read-line $in nil :eof)
                   until (eq line :eof)
                   do (progn (format t "~a~%" line)))))

   (is-expand (one:forl (line #P"path")
                (format t "~a~%" line)
                (format t "~a2~%" line))
              (with-open-file ($in #P"path"
                               :direction :input
                               :element-type 'character)
                (loop for line = (read-line $in nil :eof)
                   until (eq line :eof)
                   do (progn (format t "~a~%" line)
                             (format t "~a2~%" line))))))


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
      (is (one:for (l (testdat "strs.txt"))
            (setf s (format nil "~a ~a" s l)))
          nil)
      (is s " 1 3 5 7"))
    (let ((s ""))
      (is (one:for (l (testdat "strs.txt"))
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
