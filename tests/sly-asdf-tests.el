;;; sly-asdf-tests.el --- Tests for sly-asdf  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defvar sly-contribs nil)
(defvar sly-mrepl-shortcut-alist nil)

(defun sly-eval (&rest _args)
  "Stub replaced by individual tests."
  (error "Unexpected sly-eval call"))

(defun sly-from-lisp-filename (&rest _args)
  "Stub replaced by individual tests."
  (error "Unexpected sly-from-lisp-filename call"))

(defmacro define-sly-contrib (&rest _args)
  "Stub `define-sly-contrib' while loading the command helpers under test."
  nil)

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Compatibility definition for the package's Emacs 24.3 minimum."
    `(eval-after-load ,file '(progn ,@body))))

(provide 'sly)

(load
 (expand-file-name
  "../sly-asdf.el"
  (file-name-directory (or load-file-name buffer-file-name)))
 nil t)

(ert-deftest sly-asdf-isearch-system-searches-every-system-file ()
  (let (multi-isearch-files-argument
        single-buffer-isearch-called)
    (cl-letf (((symbol-function 'sly-eval)
               (lambda (_form) '("/tmp/one.lisp" "/tmp/two.lisp")))
              ((symbol-function 'sly-from-lisp-filename) #'identity)
              ((symbol-function 'find-file-noselect) #'identity)
              ((symbol-function 'isearch-forward)
               (lambda () (setq single-buffer-isearch-called t)))
              ((symbol-function 'multi-isearch-files)
               (lambda (files) (setq multi-isearch-files-argument files))))
      (sly-asdf-isearch-system "example"))
    (should-not single-buffer-isearch-called)
    (should (equal multi-isearch-files-argument
                   '("/tmp/one.lisp" "/tmp/two.lisp")))))

;;; sly-asdf-tests.el ends here
