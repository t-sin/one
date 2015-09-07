(in-package :cl-user)
(defpackage one-test
  (:use :cl
        :one
        :prove))
(in-package :one-test)

;; NOTE: To run this test file, execute `(asdf:test-system :one)' in your Lisp.

(plan 4)

(defun testdat (path)
  (merge-pathnames path *load-pathname*))

(subtest "for --- expansion")
(subtest "forl --- expansion")



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
    (let ((s ""))
      (is (one:for (l (testdat "strs.txt"))
            (setf s (format nil "~a ~a" s l)))
          nil)
      (is s " 1 3 5 7"))
    (let ((s ""))
      (is (one:for (l (testdat "strs.txt"))
            (setf s (format nil "~a ~a" s l)))
          nil)
      (is s " the quick brown fox jumps over the red lazy dog"))))

(finalize)
