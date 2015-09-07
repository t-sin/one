(in-package :cl-user)
(defpackage one-test
  (:use :cl
        :one
        :prove))
(in-package :one-test)

;; NOTE: To run this test file, execute `(asdf:test-system :one)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
