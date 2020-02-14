(defpackage :asdf-test/tests/main
  (:use :cl)
  (:import-from :asdf-test/main))

(in-package :asdf-test/tests/main)


(rove:deftest check-foo
  (rove:testing "message is foo"
    (rove:ok (string-equal (asdf-test/main:foo) "foo"))))
