#|
  This file is a part of one project.
  Copyright (c) 2015 t-sin (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage one-test-asd
  (:use :cl :asdf))
(in-package :one-test-asd)

(defsystem one-test
  :author "t-sin"
  :license "MIT"
  :depends-on (:one
               :prove)
  :components ((:test-file "one"))
  :description "Test system for one"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
