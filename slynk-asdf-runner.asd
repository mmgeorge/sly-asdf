(asdf:defsystem :slynk-asdf-runner
  :depends-on (#:slynk #:slynk-asdf)
  :description "Runner for compiling a system in a separate process"
  :components ((:file "slynk-asdf-runner")))
