(in-package :cl-user)
(defpackage one-test
  (:use :cl
        :one
        :prove))
(in-package :one-test)

;; NOTE: To run this test file, execute `(asdf:test-system :one)' in your Lisp.

(plan 6)

(defun testdat (path)
  (merge-pathnames path *load-pathname*))


(subtest "read-for"
  (with-input-from-string (in "1 2 3 4")
    (is (one::read-for in #'read #'identity)
        '(1 2 3 4)))
  (with-input-from-string (in "1 2 3 4")
    (is (one::read-for in #'read-line #'identity)
        '("1 2 3 4")))
  (with-input-from-string (in "1 2 3 4")
    (is (one::read-for in #'read #'1+)
        '(2 3 4 5)))
  (with-input-from-string (in "a b c :eof d")
    (is (one::read-for in #'read #'identity)
        '(a b c :eof d))))

(subtest "with-input-from-file"
  (is-expand (one::with-input-from-file (var "path") body)
             (with-open-file (var "path"
                              :direction :input
                              :element-type 'character)
               body)))

(subtest "call-read-for"
  (is (one::call-read-for (namestring (testdat "nums.txt")) #'read)
      '(1 3 5 7))
  (is (one::call-read-for (testdat "nums.txt") #'read #'1+)
      '(2 4 6 8))
  (with-input-from-string (in "1 3 5 7")
    (let ((*standard-input* in))
      (is (one::call-read-for :stdin #'read)
          '(1 3 5 7))))
  (is (one::call-read-for 42 #'read)
      nil))


(subtest "for (using read)"
  (subtest "with no body"
    (is (one:for (l (testdat "nums.txt")))
        '(1 3 5 7))
    (is (one:for (l (testdat "strs.txt")))
        '(the quick brown fox
          jumps over
          the red lazy dog)))

  (subtest "with body"
    (let ((n 0))
      (is (one:for (l (testdat "nums.txt"))
            (incf n l))
          '(1 4 9 16))
      (is n (reduce #'+ '(1 3 5 7))))
    (let ((s ""))
      (one:for (l (testdat "strs.txt"))
               (setf s (format nil "~a ~a" s l)))
      (is s " THE QUICK BROWN FOX JUMPS OVER THE RED LAZY DOG"))))

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
    (is (one:for (l :stdin))
        '(1 2 3)))
  (with-stdin  ("1 2 3")
    (is (one:forl (l :stdin))
        '("1 2 3"))))


(finalize)
