(defpackage #:foo
  (:use #:cl)
  (:export #:foo))
(in-package #:foo)

(defun foo ()
  t)
(defun foo ()
    nil)
