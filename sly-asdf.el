;; ASDF project loading
;; Ported from slime-asdf.el
(require 'sly)
(require 'sly-mrepl)
(require 'cl-lib)
(require 'grep)


(define-sly-contrib sly-asdf
  "ASDF project loading"
  (:authors "Daniel Barlow       <dan@telent.net>"
            "Marco Baringer      <mb@bese.it>"
            "Edi Weitz           <edi@agharta.de>"
            "Stas Boukarev       <stassats@gmail.com>"
            "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:sly-dependencies sly-mrepl)
  (:slynk-dependencies slynk/asdf))


;;; Interactive functions

(defun sly-load-system (&optional system)
  "Compile and load an ASDF system.  
Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (sly-read-system-name)))
  (sly-oos system 'load-op))

(defun sly-reload-system (system)
  "Reload an ASDF system without reloading its dependencies."
  (interactive (list (sly-read-system-name)))
  (sly-save-some-lisp-buffers)
  ;;(sly-display-output-buffer)
  (message "Performing ASDF LOAD-OP on system %S" system)
  (sly-eval-async
      `(slynk-asdf:reload-system ,system)
    #'(lambda (result)
        (sly-compilation-finished result (current-buffer)))))

(defun sly-save-system (system)
  "Save files belonging to an ASDF system."
  (interactive (list (sly-read-system-name)))
  (sly-eval-async
      `(slynk-asdf:asdf-system-files ,system)
    (lambda (files)
      (dolist (file files)
        (let ((buffer (get-file-buffer (sly-from-lisp-filename file))))
          (when buffer
            (with-current-buffer buffer
              (save-buffer buffer)))))
      (message "Done."))))


(defun sly-browse-system (name)
  "Browse files in an ASDF system using Dired."
  (interactive (list (sly-read-system-name)))
  (sly-eval-async `(slynk-asdf:asdf-system-directory ,name)
    (lambda (directory)
      (when directory
        (dired (sly-from-lisp-filename directory))))))

(if (fboundp 'rgrep)
    (defun sly-rgrep-system (sys-name regexp)
      "Run `rgrep' on the base directory of an ASDF system."
      (interactive (progn (grep-compute-defaults)
                          (list (sly-read-system-name nil nil t)
                                (grep-read-regexp))))
      (rgrep regexp "*.lisp"
             (sly-from-lisp-filename
              (sly-eval `(slynk-asdf:asdf-system-directory ,sys-name)))))
  (defun sly-rgrep-system ()
    (interactive)
    (error "This command is only supported on GNU Emacs >21.x.")))

(if (boundp 'multi-isearch-next-buffer-function)
    (defun sly-isearch-system (sys-name)
      "Run `isearch-forward' on the files of an ASDF system."
      (interactive (list (sly-read-system-name nil nil t)))
      (let* ((files (mapcar 'sly-from-lisp-filename
                            (sly-eval `(slynk-asdf:asdf-system-files ,sys-name))))
             (multi-isearch-next-buffer-function
              (lexical-let* 
                  ((buffers-forward  (mapcar #'find-file-noselect files))
                   (buffers-backward (reverse buffers-forward)))
                #'(lambda (current-buffer wrap)
                    ;; Contrarily to the docstring of
                    ;; `multi-isearch-next-buffer-function', the first
                    ;; arg is not necessarily a buffer. Report sent
                    ;; upstream. (2009-11-17)
                    (setq current-buffer (or current-buffer (current-buffer)))
                    (let* ((buffers (if isearch-forward
                                        buffers-forward
                                      buffers-backward)))
                      (if wrap
                          (car buffers)
                        (second (memq current-buffer buffers))))))))
        (isearch-forward)))
  (defun sly-isearch-system ()
    (interactive)
    (error "This command is only supported on GNU Emacs >23.1.x.")))

(defun sly-query-replace-system (name from to &optional delimited)
  "Run `query-replace' on an ASDF system."
  (interactive (let ((system (sly-read-system-name)))
                 (cons system (sly-read-query-replace-args
                               "Query replace throughout `%s'" system))))
  (condition-case c
      ;; `tags-query-replace' actually uses `query-replace-regexp'
      ;; internally.
      (tags-query-replace (regexp-quote from) to delimited
                          '(mapcar 'sly-from-lisp-filename
                                   (sly-eval `(slynk-asdf:asdf-system-files ,name))))
    (error
     ;; Kludge: `tags-query-replace' does not actually return but
     ;; signals an unnamed error with the below error
     ;; message. (<=23.1.2, at least.)
     (unless (string-equal (error-message-string c) "All files processed")
       (signal (car c) (cdr c)))        ; resignal
     t)))

(defun sly-query-replace-system-and-dependents
    (name from to &optional delimited)
  "Run `query-replace' on an ASDF system and all the systems
depending on it."
  (interactive (let ((system (sly-read-system-name)))
                 (cons system (sly-read-query-replace-args
                               "Query replace throughout `%s'+dependencies"
                               system))))
  (sly-query-replace-system name from to delimited)
  (dolist (dep (sly-who-depends-on-rpc name))
    (when (y-or-n-p (format "Descend into system `%s'? " dep))
      (sly-query-replace-system dep from to delimited))))

(defun sly-delete-system-fasls (name)
  "Delete FASLs produced by compiling a system."
  (interactive (list (sly-read-system-name)))
  (sly-repl-shortcut-eval-async
   `(slynk-asdf:delete-system-fasls ,name)
   'message))


(defun sly-who-depends-on (system-name)
  (interactive (list (sly-read-system-name)))
  (sly-xref :depends-on system-name))




;;; Utilities

(defgroup sly-asdf nil
  "ASDF support for Sly."
  :prefix "sly-asdf-"
  :group 'sly)

(defvar sly-system-history nil
  "History list for ASDF system names.")

(defun sly-bogus-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun sly-save-some-lisp-buffers ()
  ;;(if slime-repl-only-save-lisp-buffers
  ;;(save-some-buffers nil (lambda ()
  ;;(and (memq major-mode slime-lisp-modes)
  ;;(not (null buffer-file-name)))))
  (save-some-buffers))

(defun sly-display-output-buffer ()
  "Display the output buffer and scroll to bottom."
  (with-current-buffer (sly-output-buffer)
    (goto-char (point-max))
    (unless (get-buffer-window (current-buffer) t)
      (display-buffer (current-buffer) t))
    (sly-repl-show-maximum-output)))

(defun sly-read-query-replace-args (format-string &rest format-args)
  (let* ((common (query-replace-read-args (apply #'format format-string
                                                 format-args)
                                          t t)))
    (list (nth 0 common) (nth 1 common) (nth 2 common))))

(defun sly-read-system-name (&optional prompt 
                                       default-value
                                       determine-default-accurately)
  "Read a system name from the minibuffer, prompting with PROMPT.
If no `default-value' is given, one is tried to be determined: if
`determine-default-accurately' is true, by an RPC request which
grovels through all defined systems; if it's not true, by looking
in the directory of the current buffer."
  (let* ((completion-ignore-case nil)
         (prompt (or prompt "System"))
         (system-names (sly-eval `(slynk-asdf:list-asdf-systems)))
         (default-value
           (or default-value 
               (if determine-default-accurately
                   (sly-determine-asdf-system (buffer-file-name)
                                              (sly-current-package))
                 (sly-find-asd-file (or default-directory
                                        (buffer-file-name))
                                    system-names))))
         (prompt (concat prompt (if default-value
                                    (format " (default `%s'): " default-value)
                                  ": "))))
    (completing-read prompt (sly-bogus-completion-alist system-names)
                     nil nil nil
                     'sly-system-history default-value)))

(defun sly-find-asd-file (directory system-names)
  "Tries to find an ASDF system definition file in the
`directory' and returns it if it's in `system-names'."
  (let ((asd-files
         (directory-files (file-name-directory directory) nil "\.asd$")))
    (cl-loop for system in asd-files
             for candidate = (file-name-sans-extension system)
             when (cl-find candidate system-names :test #'string-equal)
             do (cl-return candidate))))

(defun sly-determine-asdf-system (filename buffer-package)
  "Try to determine the asdf system that `filename' belongs to."
  (sly-eval
   `(slynk-asdf:asdf-determine-system ,(and filename
                                            (sly-to-lisp-filename filename))
                                      ,buffer-package)))

(defun sly-who-depends-on-rpc (system)
  (sly-eval `(slynk-asdf:who-depends-on ,system)))

(defcustom sly-asdf-collect-notes t
  "Collect and display notes produced by the compiler.

See also `sly-highlight-compiler-notes' and
`sly-compilation-finished-hook'."
  :group 'sly-asdf)

(defun sly-asdf-operation-finished-function (system)
  (if sly-asdf-collect-notes
      #'sly-compilation-finished
    (sly-curry (lambda (system result)
                 (let (sly-highlight-compiler-notes
                       sly-compilation-finished-hook)
                   (sly-compilation-finished result)))
               system)))

(defun sly-oos (system operation &rest keyword-args)
  "Operate On System."
  (sly-save-some-lisp-buffers)
  ;;(sly-display-output-buffer)
  (message "Performing ASDF %S%s on system %S"
           operation (if keyword-args (format " %S" keyword-args) "")
           system)
  (sly-eval-async
      `(slynk-asdf:operate-on-system-for-emacs ,system ',operation ,@keyword-args)
    #'(lambda (result)
        (sly-compilation-finished result (current-buffer)))))

(provide 'sly-asdf)
