(in-package :cl-user)
(defpackage :one/tests/io
  (:use :cl :rove))
(in-package :one/tests/io)

(deftest util-read*-test
  (with-input-from-string (in "wannyan spacetime odessey")
    (ok (equal (one/io:read* in) 'wannyan))
    (ok (equal (one/io:read* in) 'spacetime))
    (ok (equal (one/io:read* in) 'odessey))
    (ok (equal (one/io:read* in) :eof))))

(deftest util-read-char*-test
  (with-input-from-string (in "ichi")
    (ok (equal (one/io:read-char* in) #\i))
    (ok (equal (one/io:read-char* in) #\c))
    (ok (equal (one/io:read-char* in) #\h))
    (ok (equal (one/io:read-char* in) #\i))
    (ok (equal (one/io:read-char* in) :eof))))

(deftest util-read-line*-test
  (with-input-from-string (in (format nil "doraemon~%the~%movie"))
    (ok (equal (one/io:read-line* in) "doraemon"))
    (ok (equal (one/io:read-line* in) "the"))
    (ok (equal (one/io:read-line* in) "movie"))
    (ok (equal (one/io:read-line* in) :eof))))

(deftest util-print*-test
  (with-output-to-string (out)
    (let ((*standard-output* out))
      (one/io:print* 42))
    (ok (equal (get-output-stream-string out) "42
"))))
