#|
  This file is a part of one project.
  Copyright (c) 2015 t-sin (shinichi.tanaka45@gmail.com)
|#

#|
  Utilities for one-liner

  Author: t-sin (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage one-asd
  (:use :cl :asdf))
(in-package :one-asd)

(defsystem one
  :version "0.1"
  :author "t-sin"
  :license "MIT"
  :depends-on ()
  :components ((:file "one"))
  :description "Utilities for one-liner"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op one-test))))
