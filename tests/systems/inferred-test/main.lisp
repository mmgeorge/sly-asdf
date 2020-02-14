(defpackage :inferred-test/main
  (:use :cl))

(in-package :inferred-test/main)



(defun foo-add (a b)
  (+ a b))


(defun ignore-var (ignored)
  (+ 1 1))
