(in-package :cl-user)
(defpackage :one/tests/core
  (:use :cl :rove))
(in-package :one/tests/core)


(defun make-test-read-fn ()
  (let ((buffer))
    (values (lambda (stream)
              (let ((value (read-line stream nil :eof)))
                (push value buffer)
                value))
            (lambda () buffer))))

(deftest internal-operator-scan-test
  (testing "stream"
    (testing "function which is returned by `$scan`"
      (ok (typep (one::$scan (make-string-input-stream "hogehoge") #'one:read-line*)
                 'function)))

    (testing "arity of returned function is 1"
      (ng (signals (with-input-from-string (in "hogehoge")
                     (funcall (one/core:$scan in #'one:read-line*)))
                   'simple-error))
      (ok (null (with-input-from-string (in "hogehoge")
                  (funcall (one/core:$scan in #'one:read-line*)
                           #'identity))))
      (ng (signals (with-input-from-string (in "hogehoge")
                     (funcall (one/core:$scan in #'one:read-line*)
                              #'identity 42))
                   'simple-error)))

    (testing "read-fn specified is used"
      (multiple-value-bind (read-fn get-fn)
          (make-test-read-fn)
        (with-input-from-string (in "nobita-san")
          (funcall (one/core:$scan in read-fn) #'identity))
        (ok (equal (funcall get-fn) '(:eof "nobita-san")))))

    (testing "read-fn must be returns :eof when stream end, or signals error")
    (testing "op is called for all stream elements"))

  (testing "pathname"
    (testing "function which is returned by `$scan`")
    (testing "arity of returned function is 1")
    (testing "read-fn specified is used")
    (testing "read-fn must be returns :eof when stream end, or signals error")
    (testing "op is called for all stream elements"))

  (testing "sequence"
    (testing "function which is returned by `$scan`")
    (testing "arity of returned function is 1")
    (testing "next-fn specified is used")
    (testing "op is called for all stream elements")))

(deftest internal-operator-call-if-test
  (diag "ok"))

(deftest internal-operator-gather-test
  (diag "ok"))

(deftest internal-operator-fold-test
  (diag "ok"))
