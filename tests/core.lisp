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
      (ok (signals (with-input-from-string (in "wan nyan")
                     (funcall (one/core:$scan in #'one:read-line*)))
                   'error))
      (ok (null (with-input-from-string (in "wan nyan")
                  (funcall (one/core:$scan in #'one:read-line*)
                           #'identity))))
      (ok (signals (with-input-from-string (in "wan nyan")
                     (funcall (one/core:$scan in #'one:read-line*)
                              #'identity 25))
                   'error)))

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
      (ok (signals (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
                     (funcall (one/core:$scan in #'one:read-line*)))
                   'error))
      (ok (null (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
                  (funcall (one/core:$scan in #'one:read-line*)
                           #'identity))))
      (ok (signals (with-open-file (in (asdf:system-relative-pathname :one "tests/data.txt"))
                     (funcall (one/core:$scan in #'one:read-line*)
                              #'identity 25))
                   'error)))

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
      (ok (signals (funcall (one/core:$scan '(1 2 3 4) #'cdr)) 'error))
      (ok (null (funcall (one/core:$scan '(1 2 3 4) #'cdr) #'identity)))
      (ok (signals (funcall (one/core:$scan '(1 2 3 4) #'cdr)
                            #'identity 25)
                   'error)))

    (testing "next-fn specified is used"
      (multiple-value-bind (op get-fn)
          (make-get-op)
        (funcall (one/core:$scan '(1 2 3 4) #'cddr) op)
        (ok (equal (funcall get-fn) '(3 1)))))

    (testing "op is called for all stream elements"
      (let ((s "ichi"))
        (funcall (one/core:$scan s #'identity) (make-chareq-op s))))))


(defun make-call-if-test-op ()
  (let ((called nil))
    (values (lambda (input) (setf called input))
            (lambda () called))))

(deftest internal-operator-call-if-test
  (testing "function is returned when `$call-if` is called"
    (ok (typep (one/core:$call-if #'identity #'identity) 'function)))

  (testing "arity of returned function is 1, that is input"
    (ok (signals (funcall (one/core:$call-if #'identity #'identity))
                 'error))
    (diag "`$call-if` returns evaluated value, but don't care")
    (ok (funcall (one/core:$call-if #'identity #'identity) "ichi"))
    (ok (signals (funcall (one/core:$call-if #'identity #'identity) "ichi" "hachi")
                 'error)))

  (testing "called successor operation when input is true by predicate"
    (multiple-value-bind (op get-fn)
        (make-call-if-test-op)
      (funcall (one/core:$call-if #'zerop op) 0)
      (ok (eq (funcall get-fn) 0)))
    (multiple-value-bind (op get-fn)
        (make-call-if-test-op)
      (funcall (one/core:$call-if #'zerop op) 1)
      (ok (null (funcall get-fn))))))

(deftest internal-operator-gather-test
  (testing "`$gather` returns two functions"
    (ok (eq (length (multiple-value-list (one/core:$gather #'identity))) 2))
    (multiple-value-bind (slurp barf)
        (one/core:$gather #'identity)
      (ok (typep slurp 'function))
      (ok (typep barf 'function))))

  (testing "arity of 'slurp' is 1, input from previous operation"
    (multiple-value-bind (slurp barf)
        (one/core:$gather #'identity)
      (declare (ignore barf))
      (ok (signals (funcall slurp) 'error))
      (ok (funcall slurp 1))
      (ok (signals (funcall slurp 1 2) 'error))))

  (testing "arity of 'barf' is 1, successor operation"
    (multiple-value-bind (slurp barf)
        (one/core:$gather #'identity)
      (funcall slurp 1)
      (ok (signals (funcall barf) 'error))
      (ok (funcall barf #'identity))
      (ok (signals (funcall barf #'identity 2) 'error))))

  (testing "gathering operation do something like reduce"
    (testing "gathering operation pass through gathered inputs"
      (multiple-value-bind (slurp barf)
          (one/core:$gather #'identity)
        (funcall slurp "nobita")
        (funcall slurp "doraemon")
        (funcall slurp "shizuka")
        (funcall slurp "takeshi")
        (funcall slurp "suneo")
        (ok (equal (funcall barf #'identity) '("nobita" "doraemon" "shizuka" "takeshi" "suneo")))))
    (testing "gathering with sorting"
      (multiple-value-bind (slurp barf)
          (one/core:$gather (lambda (lis) (sort lis #'string<)))
        (funcall slurp "nobita")
        (funcall slurp "doraemon")
        (funcall slurp "shizuka")
        (funcall slurp "takeshi")
        (funcall slurp "suneo")
        (ok (equal (funcall barf #'identity)
                   '("doraemon" "nobita" "shizuka" "suneo" "takeshi")))))
    (testing "gathering with concatenation"
      (multiple-value-bind (slurp barf)
          (one/core:$gather (lambda (lis) (format nil "~{~a~^-~}" lis)))
        (funcall slurp "nobita")
        (funcall slurp "doraemon")
        (funcall slurp "shizuka")
        (funcall slurp "takeshi")
        (funcall slurp "suneo")
        (ok (string= (funcall barf #'identity)
                   "nobita-doraemon-shizuka-takeshi-suneo")))))

  (testing "when calling 'barf', successor operation is applied to buffered input"
    (testing "identity"
      (multiple-value-bind (slurp barf)
          (one/core:$gather #'identity)
        (funcall slurp "nobita")
        (funcall slurp "doraemon")
        (funcall slurp "shizuka")
        (funcall slurp "takeshi")
        (funcall slurp "suneo")
        (ok (equal (funcall barf #'identity)
                   '("nobita" "doraemon" "shizuka" "takeshi" "suneo")))))
    (testing "sorting after gathering; do same above"
      (multiple-value-bind (slurp barf)
          (one/core:$gather #'identity)
        (funcall slurp "nobita")
        (funcall slurp "doraemon")
        (funcall slurp "shizuka")
        (funcall slurp "takeshi")
        (funcall slurp "suneo")
        (ok (equal (funcall barf (lambda (lis) (sort lis #'string<)))
                   '("doraemon" "nobita" "shizuka" "suneo" "takeshi")))))
    (testing "concatenation after gathering; do same above"
      (multiple-value-bind (slurp barf)
          (one/core:$gather #'identity)
        (funcall slurp "nobita")
        (funcall slurp "doraemon")
        (funcall slurp "shizuka")
        (funcall slurp "takeshi")
        (funcall slurp "suneo")
        (ok (string= (funcall barf (lambda (lis) (format nil "~{~a~^-~}" lis)))
                   "nobita-doraemon-shizuka-takeshi-suneo"))))))

(deftest internal-operator-fold-test
  (testing "`$fold` returns two functions"
    (multiple-value-bind (slurp barf)
        (one/core:$fold (lambda (x y) (declare (ignore x)) y) nil)
      (ok (typep slurp 'function))
      (ok (typep barf 'function))))

  (testing "arity of 'slurp' is 1, input from previous operation"
    (multiple-value-bind (slurp barf)
        (one/core:$fold (lambda (x y) (declare (ignore x)) y) nil)
      (declare (ignore barf))
      (ok (signals (funcall slurp) 'error))
      (ok (funcall slurp 1))
      (ok (signals (funcall slurp 2 1) 'error))))

  (testing "arity of 'barf' is 1, successor operation"
    (multiple-value-bind (slurp barf)
        (one/core:$fold (lambda (x y) (declare (ignore x)) y) nil)
      (funcall slurp 1)
      (ok (signals (funcall barf) 'error))
      (ok (funcall barf #'identity))
      (ok (signals (funcall barf #'identity 2) 'error))))

  (testing "folding operation do something like reduce"
    (testing "fold-op takes two parameter: accumrator initialized init-value and input"
      (ok (signals (funcall (one/core:$fold (lambda (x) x) nil) 0) 'error))
      (ok (signals (funcall (one/core:$fold (lambda (x y) (declare (ignore x)) y) nil))))
      (ok (signals (funcall (one/core:$fold (lambda (x y z)  (declare (ignore x y)) z) nil) 'error))))

    (testing "folding operation pass initial value through in folding process"
      (multiple-value-bind (slurp barf)
          (one/core:$fold (lambda (x y) (declare (ignore y)) x) "ichi")
        (funcall slurp "hachi")
        (funcall slurp "chiko")
        (funcall slurp "bulltaro")
        (funcall slurp "daku")
        (ok (equal (funcall barf #'identity) "ichi")))

      (testing "folding operation with concatenation"
        (multiple-value-bind (slurp barf)
            (one/core:$fold (lambda (x y) (format nil "~a-~a" x y)) "")
          (funcall slurp "hachi")
          (funcall slurp "chiko")
          (funcall slurp "bulltaro")
          (funcall slurp "daku")
          (ok (equal (funcall barf #'identity) "-hachi-chiko-bulltaro-daku"))))))

  (testing "successor operation is called"
    (multiple-value-bind (slurp barf)
       (one/core:$fold (lambda (x y) (declare (ignore x)) y) nil)
      (funcall slurp "hachi")
      (ok (equal (funcall barf (lambda (x) (format nil "~a!!!!" x))) "hachi!!!!")))))
