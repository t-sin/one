(in-package :cl-user)
(defpackage :one/tests/core
  (:use :cl :rove))
(in-package :one/tests/core)

(deftest internal-operator-scan-test
  (testing "stream"
    (testing "function which is returned by `$scan`"
      (ok (typep (one::$scan (make-string-input-stream "hogehoge") #'one:read-line*)
                 'function)))

    (testing "arity of returned function is 1"
      (ng (signals
           (funcall (one::$scan (make-string-input-stream "hogehoge") #'one:read-line*))
           'simple-error))
      (ok (null (funcall (one::$scan (make-string-input-stream "hogehoge") #'one:read-line*)
                         #'identity)))
      (ng (signals
           (funcall (one::$scan (make-string-input-stream "hogehoge") #'one:read-line*)
                    #'identity 42)
           'simple-error)))

    (testing "read-fn which specified is used")
    (testing "read-fn must be returns :eof when stream end")
    (testing "op is called for all stream elements"))

  (testing "pathname")
  (testing "sequence"))

(deftest internal-operator-call-if-test
  (diag "ok"))

(deftest internal-operator-gather-test
  (diag "ok"))

(deftest internal-operator-fold-test
  (diag "ok"))
