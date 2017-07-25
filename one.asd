#|
  This file is a part of one project.
  Copyright (c) 2017 t-sin (shinichi.tanaka45@gmail.com)
|#

#|
  Input processing framework

  Author: t-sin (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage one-asd
  (:use :cl :asdf))
(in-package :one-asd)

(defsystem :one
  :class :package-inferred-system
  :description "Input processing framework"
  :version "0.1"
  :author "Shinichi TANAKA"
  :license "MIT"
  :depends-on ("one/one")
  :in-order-to ((test-op (test-op :one/tests))))

(defsystem :one/tests
  :class :package-inferred-system
  :depends-on ("rove")
  :perform (test-op (o c) (uiop:symbol-call :rove ':run c)))
