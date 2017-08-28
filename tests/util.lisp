(in-package :cl-user)
(defpackage :one/tests/util
  (:use :cl :rove))
(in-package :one/tests/util)

(deftest util-read*-test
  (with-input-from-string (in "wannyan spacetime odessey")
    (ok (equal (one/util:read* in) 'wannyan))
    (ok (equal (one/util:read* in) 'spacetime))
    (ok (equal (one/util:read* in) 'odessey))
    (ok (equal (one/util:read* in) :eof))))

(deftest util-read-char*-test
  (with-input-from-string (in "ichi")
    (ok (equal (one/util:read-char* in) #\i))
    (ok (equal (one/util:read-char* in) #\c))
    (ok (equal (one/util:read-char* in) #\h))
    (ok (equal (one/util:read-char* in) #\i))
    (ok (equal (one/util:read-char* in) :eof))))

(deftest util-read-line*-test
  (with-input-from-string (in (format nil "doraemon~%the~%movie"))
    (ok (equal (one/util:read-line* in) "doraemon"))
    (ok (equal (one/util:read-line* in) "the"))
    (ok (equal (one/util:read-line* in) "movie"))
    (ok (equal (one/util:read-line* in) :eof))))

(deftest util-print*-test
  (with-output-to-string (out)
    (let ((*standard-output* out))
      (one/util:print* 42))
    (ok (equal (get-output-stream-string out) "42
"))))
