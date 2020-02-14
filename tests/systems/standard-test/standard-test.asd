(asdf:defsystem "standard-test"
    :serial t
    :components ((:file "dep")
                 (:file "foo" :depends-on ("dep"))))
