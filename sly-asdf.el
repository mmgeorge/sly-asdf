;;; sly-asdf.el --- ASDF system support for SLY -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; URL: https://github.com/mmgeorge/sly-asdf
;; Keywords: languages, lisp, sly, asdf
;; Package-Requires: ((emacs "24.3")(sly "1.0.0-beta2"))
;; Maintainer: Matt George <mmge93@gmail.com>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; `sly-asdf` is an external contrib for SLY that provides additional
;; support for working with asdf projects.  Ported from the original slime
;; contrib.  See <https://github.com/slime/slime/blob/master/contrib/slime-asdf.el>.
;;
;; See README.md
;;
;;; Code:

(require 'sly)
(require 'cl-lib)
(require 'grep)


(defvar sly-mrepl-shortcut-alist) ;; declared in sly-mrepl
(defvar sly-asdf-find-system-file-max-depth 10
  "Max recursive depth for finding an asd system definition from the current directory.")
(defvar sly-asdf-shortcut-alist
  '(("load-system" . sly-asdf-load-system)
    ("reload-system" . sly-asdf-reload-system)
    ("browse-system" . sly-asdf-browse-system)
    ("open-system" . sly-asdf-open-system)
    ("save-system" . sly-asdf-save-system)))


(define-sly-contrib sly-asdf
  "ASDF system support"
  (:authors "Daniel Barlow       <dan@telent.net>"
            "Marco Baringer      <mb@bese.it>"
            "Edi Weitz           <edi@agharta.de>"
            "Stas Boukarev       <stassats@gmail.com>"
            "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slynk-dependencies slynk-asdf)
  (:on-load
   (setq sly-mrepl-shortcut-alist
         (append sly-mrepl-shortcut-alist sly-asdf-shortcut-alist))))


;;; Interactive functions

(defun sly-asdf-load-system (&optional system)
  "Compile and load an ASDF SYSTEM.
Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (sly-asdf-read-system-name)))
  (sly-asdf-oos system 'load-op))


(defun sly-asdf-reload-system (system)
  "Compile and load an ASDF SYSTEM without reloading dependencies.
Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (sly-asdf-read-system-name)))
  (sly-asdf-save-some-lisp-buffers)
  ;;(sly-asdf-display-output-buffer)
  (message "Performing ASDF LOAD-OP on system %S" system)
  (sly-eval-async
      `(slynk-asdf:reload-system ,system)
    #'(lambda (result)
        (sly-compilation-finished result (current-buffer)))))


(defun sly-asdf-save-system (system)
  "Save files belonging to an ASDF SYSTEM."
  (interactive (list (sly-asdf-read-system-name)))
  (sly-eval-async
      `(slynk-asdf:asdf-system-files ,system)
    (lambda (files)
      (dolist (file files)
        (let ((buffer (get-file-buffer (sly-from-lisp-filename file))))
          (when buffer
            (with-current-buffer buffer
              (save-buffer buffer)))))
      (message "Done."))))


(defun sly-asdf-browse-system (name)
  "Browse files in an ASDF system NAME using Dired."
  (interactive (list (sly-asdf-read-system-name)))
  (sly-eval-async `(slynk-asdf:asdf-system-directory ,name)
    (lambda (directory)
      (when directory
        (dired (sly-from-lisp-filename directory))))))

(defun sly-asdf-open-system (name &optional load interactive)
  "Open all files implicated in an ASDF system, in separate emacs buffers."
  (interactive (list (sly-asdf-read-system-name) nil t))
  (when (or load
            (and interactive
                 (not (sly-eval `(slynk-asdf:asdf-system-loaded-p ,name)))
                 (y-or-n-p "Load it? ")))
    (sly-asdf-load-system name))
  (sly-eval-async
      `(slynk-asdf:asdf-system-files ,name)
    (lambda (files)
      (when files
        (let ((files (mapcar 'sly-from-lisp-filename
                             (nreverse files))))
          (find-file-other-window (car files))
          (mapc 'find-file (cdr files)))))))

(defun sly-asdf-rgrep-system (sys-name regexp)
  "Run `rgrep' for REGEXP for SYS-NAME on the base directory of an ASDF system."
  (interactive (progn (grep-compute-defaults)
                      (list (sly-asdf-read-system-name nil nil)
                            (grep-read-regexp))))
  (rgrep regexp "*.lisp"
         (sly-from-lisp-filename
          (sly-eval `(slynk-asdf:asdf-system-directory ,sys-name)))))


(defun sly-asdf-isearch-system (sys-name)
  "Run function `isearch-forward' on the files of an ASDF system SYS-NAME."
  (interactive (list (sly-asdf-read-system-name nil nil)))
  (let* ((files (mapcar 'sly-from-lisp-filename
                        (sly-eval `(slynk-asdf:asdf-system-files ,sys-name))))
         (multi-isearch-next-buffer-function
          (let*
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
                    (cl-second (memq current-buffer buffers))))))))
    (isearch-forward)))


(defun sly-asdf-query-replace-system (_name from to &optional delimited)
  "Run `query-replace' on an ASDF system with NAME given FROM and TO with optional DELIMITED."
  ;; MG: Underscore added to _name to suppress an unused-lexical-arg warning that fires
  ;; despite the var being used in the condition-case below.
  (interactive (let ((system (sly-asdf-read-system-name)))
                 (cons system (sly-asdf-read-query-replace-args
                               "Query replace throughout `%s'" system))))
  (condition-case c
      ;; `tags-query-replace' actually uses `query-replace-regexp'
      ;; internally.
      (tags-query-replace (regexp-quote from) to delimited
                          '(mapcar 'sly-from-lisp-filename
                                   (sly-eval `(slynk-asdf:asdf-system-files ,_name))))
    (error
     ;; Kludge: `tags-query-replace' does not actually return but
     ;; signals an unnamed error with the below error
     ;; message. (<=23.1.2, at least.)
     (unless (string-equal (error-message-string c) "All files processed")
       (signal (car c) (cdr c)))        ; resignal
     t)))


(defun sly-asdf-query-replace-system-and-dependents
    (name from to &optional delimited)
  "Run `query-replace' on an ASDF system with NAME given FROM and TO.
DELIMITED is optional.  Includes the base system and all other systems it depending on it."
  (interactive (let ((system (sly-asdf-read-system-name)))
                 (cons system (sly-asdf-read-query-replace-args
                               "Query replace throughout `%s'+dependencies"
                               system))))
  (sly-asdf-query-replace-system name from to delimited)
  (dolist (dep (sly-asdf-who-depends-on-rpc name))
    (when (y-or-n-p (format "Descend into system `%s'? " dep))
      (sly-asdf-query-replace-system dep from to delimited))))


(defun sly-asdf-delete-system-fasls (name)
  "Delete FASLs produced by compiling a system with NAME."
  (interactive (list (sly-asdf-read-system-name)))
  (sly-eval-async
   `(slynk-asdf:delete-system-fasls ,name)
   'message))


(defun sly-asdf-who-depends-on (sys-name)
  "Determine who depends on system with SYS-NAME."
  (interactive (list (sly-asdf-read-system-name)))
  (sly-xref :depends-on sys-name))


;;; Utilities

(defgroup sly-asdf nil
  "ASDF support for Sly."
  :prefix "sly-asdf-"
  :group 'sly)


(defvar sly-asdf-system-history nil
  "History list for ASDF system names.")


(defun sly-asdf-bogus-completion-alist (list)
  "Make an alist out of LIST.
The same elements go in the CAR, and nil in the CDR.  To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))


(defun sly-asdf-save-some-lisp-buffers ()
  "Compatability."
  ;;(if slime-repl-only-save-lisp-buffers
  ;;(save-some-buffers nil (lambda ()
  ;;(and (memq major-mode slime-lisp-modes)
  ;;(not (null buffer-file-name)))))
  (save-some-buffers))


(defun sly-asdf-read-query-replace-args (format-string &rest format-args)
  "Read query args, displaying FORMAT-STRING with FORMAT-ARGS."
  (let* ((common (query-replace-read-args (apply #'format format-string
                                                 format-args)
                                          t t)))
    (list (nth 0 common) (nth 1 common) (nth 2 common))))


(defun sly-asdf-read-system-name (&optional prompt default-value)
  "Read a system name from the minibuffer, prompting with PROMPT.
If no DEFAULT-VALUE is given, one is tried to be determined: if
DETERMINE-DEFAULT-ACCURATELY is true, by an RPC request which
grovels through all defined systems; if it's not true, by looking
in the directory of the current buffer."
  (let* ((completion-ignore-case nil)
         (prompt (or prompt "System"))
         (system-names (sly-eval `(slynk-asdf:list-asdf-systems)))
         (default-value
           (or default-value (sly-asdf-find-current-system) (car sly-asdf-system-history)))
         (prompt (concat prompt (if default-value
                                    (format " (default `%s'): " default-value)
                                  ": "))))
    (let ((history-delete-duplicates t))
      (completing-read prompt (sly-asdf-bogus-completion-alist system-names)
                       nil nil nil
                       'sly-asdf-system-history default-value))))


(defun sly-asdf-find-current-system ()
  "Find the name of the current asd system."
  (let ((system-file (sly-asdf-find-system-file default-directory)))
    (when system-file
      (file-name-base system-file))))


(cl-defun sly-asdf-find-system-file (directory &optional (depth sly-asdf-find-system-file-max-depth))
  "Find the first file in the current DIRECTORY or a parent of DIRECTORY that includes a .asd file."
  (let ((fname (directory-file-name directory)))
    (or
     (cl-find-if #'(lambda (file) (string-equal "asd" (file-name-extension file)))
                 (directory-files directory))
     (and (> depth 0)
          (file-name-directory fname)
          (sly-asdf-find-system-file (file-name-directory fname) (1- depth))))))


(defun sly-asdf-determine-asdf-system (filename buffer-package)
  "Try to determine the asdf system provided BUFFER-PACKAGE that FILENAME belongs to."
  (sly-eval
   `(slynk-asdf:asdf-determine-system ,(and filename
                                            (sly-to-lisp-filename filename))
                                      ,buffer-package)))


(defun sly-asdf-who-depends-on-rpc (system)
  "Find who depends on RPC for SYSTEM."
  (sly-eval `(slynk-asdf:who-depends-on ,system)))


(defun sly-asdf-oos (system operation &rest keyword-args)
  "Operate On System.  Apply the given OPERATION on SYSTEM provided KEYWORD-ARGS."
  (message "Performing ASDF %S%s on system %S"
           operation (if keyword-args (format " %S" keyword-args) "")
           system)
  (sly-eval-async
      `(slynk-asdf:operate-on-system-for-emacs ,system ',operation ,@keyword-args)
    #'(lambda (result)
        (sly-compilation-finished result (current-buffer)))))


;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-asdf 'append))


(provide 'sly-asdf)
;;; sly-asdf.el ends here
