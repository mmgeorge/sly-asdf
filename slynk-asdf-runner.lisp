(defpackage :slynk-asdf-runner
  (:use :cl)
  (:import-from :slynk-asdf)
  (:export #:check-for-compilation-errors))


(in-package :slynk-asdf-runner)


(defun check-for-compilation-errors (system)
  (print-object
     (let ((asdf/driver:*output-translation-function* (slynk-asdf:make-output-translation-function))
           (*standard-output* (make-string-output-stream)))
            (slynk-asdf:while-collecting-notes (:interactive nil)
              (slynk-asdf::operate-on-system-for-emacs system 'load-op :force t)))
       *standard-output*))

