(in-package :cl-user)
(defpackage :one/tests/core
  (:use :cl :rove))
(in-package :one/tests/core)

(deftest internal-operator-scan-test
  (diag "ok"))

(deftest internal-operator-call-if-test
  (diag "ok"))

(deftest internal-operator-gather-test
  (diag "ok"))

(deftest internal-operator-fold-test
  (diag "ok"))
