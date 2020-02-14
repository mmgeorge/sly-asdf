(defsystem :asdf-test
    :class :package-inferred-system
    :depends-on ("asdf-test/main")
    :in-order-to ((test-op (test-op "asdf-test/tests"))))


(defsystem :asdf-test/tests
    :class :package-inferred-system
    :depends-on ("rove"
                 "asdf-test/tests/main")
    :perform (test-op (o c) (symbol-call :rove '#:run c)))
    
