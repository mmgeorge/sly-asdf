(defpackage :asdf-test/main
  (:use :cl)
  (:export #:foo))

(in-package :asdf-test/main)


(defun foo () "foo")
