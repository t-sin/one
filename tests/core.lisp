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

(defun make-chareq-op (s)
  (let ((idx 0))
    (lambda (in)
      (diag (format nil "actually: ~s, expected: ~s~%" in (char s idx)))
      (ok (eq in (char s idx)))
      (incf idx))))

(defun make-get-op ()
  (let ((buffer))
    (values (lambda (input) (push input buffer))
            (lambda () buffer))))

(deftest internal-operator-scan-test
  (testing "stream"
    (testing "function which is returned by `$scan`"
      (with-input-from-string (in "wan nyan")
        (ok (typep (one/core:$scan in #'one:read-line*) 'function))))

    (testing "arity of returned function is 1"
      (ng (signals (with-input-from-string (in "wan nyan")
                     (funcall (one/core:$scan in #'one:read-line*)))
                   'simple-error))
      (ok (null (with-input-from-string (in "wan nyan")
                  (funcall (one/core:$scan in #'one:read-line*)
                           #'identity))))
      (ng (signals (with-input-from-string (in "wan nyan")
                     (funcall (one/core:$scan in #'one:read-line*)
                              #'identity 25))
                   'simple-error)))

    (testing "read-fn specified is used"
      (multiple-value-bind (read-fn get-fn)
          (make-test-read-fn)
        (with-input-from-string (in "nobita-san")
          (funcall (one/core:$scan in read-fn) #'identity))
        (ok (equal (funcall get-fn) '(:eof "nobita-san")))))

    (testing "read-fn must be returns :eof when stream end, or signals error"
      (with-input-from-string (in "nobita-san")
        (ok (null (funcall (one/core:$scan in (lambda (stream) (read-char stream nil :eof)))
                           #'identity))))
      (with-input-from-string (in "nobita-san")
        (ok (signals (funcall (one/core:$scan in #'read-char) #'identity)
                     'error))))

    (testing "op is called for all stream elements")
    (let ((s "hachi"))
      (with-input-from-string (in s)
        (funcall (one/core:$scan in #'one:read-char*)
                 (make-chareq-op s)))))

  (testing "pathname"
    (testing "function which is returned by `$scan`"
      (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
        (ok (typep (one/core:$scan in #'one:read-line*) 'function))))

    (testing "arity of returned function is 1"
      (ng (signals (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
                     (funcall (one/core:$scan in #'one:read-line*)))
                   'simple-error))
      (ok (null (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
                  (funcall (one/core:$scan in #'one:read-line*)
                           #'identity))))
      (ng (signals (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
                     (funcall (one/core:$scan in #'one:read-line*)
                              #'identity 25))
                   'simple-error)))

    (testing "read-fn specified is used"
      (multiple-value-bind (read-fn get-fn)
          (make-test-read-fn)
        (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
          (funcall (one/core:$scan in read-fn) #'identity))
        (print (equal (funcall get-fn) '(:eof "nyan" "wan")))))

    (testing "read-fn must be returns :eof when stream end, or signals error"
      (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
        (ok (null (funcall (one/core:$scan in (lambda (stream) (read-char stream nil :eof)))
                           #'identity))))
      (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
        (ok (signals (funcall (one/core:$scan in #'read-char) #'identity)
                     'error))))

    (testing "op is called for all stream elements"
      (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
        (funcall (one/core:$scan in #'one:read-char*)
                 (make-chareq-op (format nil "wan~%nyan~%"))))))

  (testing "sequence"
    (testing "function which is returned by `$scan`"
      (ok (typep (one/core:$scan '(1 2 3 4) #'cdr) 'function)))

    (testing "arity of returned function is 1"
      (ng (signals (funcall (one/core:$scan '(1 2 3 4) #'cdr)) 'simple-error))
      (ok (null (funcall (one/core:$scan '(1 2 3 4) #'cdr) #'identity)))
      (ng (signals (funcall (one/core:$scan '(1 2 3 4) #'cdr)
                            #'identity 25)
                   'simple-error)))

    (testing "next-fn specified is used"
      (multiple-value-bind (op get-fn)
          (make-get-op)
        (funcall (one/core:$scan '(1 2 3 4) #'cddr) op)
        (ok (equal (funcall get-fn) '(3 1)))))

    (testing "op is called for all stream elements"
      (let ((s "ichi"))
        (funcall (one/core:$scan s #'identity) (make-chareq-op s))))))

(deftest internal-operator-call-if-test
  (diag "ok"))

(deftest internal-operator-gather-test
  (diag "ok"))

(deftest internal-operator-fold-test
  (diag "ok"))
