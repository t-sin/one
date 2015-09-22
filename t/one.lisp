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



(subtest "with-input-from-file"
  (is-expand (one::with-input-from-file (var "path") body)
             (with-open-file (var "path"
                              :direction :input
                              :element-type 'character)
               body)))


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
