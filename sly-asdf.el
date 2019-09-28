;;; sly-asdf.el --- ASDF system support for SLY -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; URL: https://github.com/mmgeorge/sly-asdf
;; Keywords: languages, lisp, sly, asdf
;; Package-Requires: ((emacs "24.3")(sly "1.0.0-beta2")(popup "0.5.3"))
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
(require 'popup)
(require 'flymake)

(defvar sly-mrepl-shortcut-alist) ;; declared in sly-mrepl
(defvar sly-asdf-enable-flymake 1
  "Opt-in to experimental flymake integration.  Currently only tested on SBCL.")
(defvar sly-asdf-find-system-file-max-depth 10
  "Max recursive depth for finding an asd system definition from the current directory.")
(defvar sly-asdf-shortcut-alist
  '(("load-system" . sly-asdf-load-system)
    ("reload-system" . sly-asdf-reload-system)
    ("browse-system" . sly-asdf-browse-system)
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
   (add-hook 'sly-connected-hook #'sly-asdf-flymake-init)
   (setq sly-mrepl-shortcut-alist
         (append sly-mrepl-shortcut-alist sly-asdf-shortcut-alist))))


;; Flymake support

(defvar *sly-asdf-lisp-extensions* (list "lisp")
  "File extensions to look for when finding open Lisp files.")
(defvar *sly-asdf--last-point* nil)
(defvar *sly-asdf--flymake-backend-state* nil)
(defvar sly-asdf--after-oos-hook nil)

(defun sly-asdf-flymake-init ()
  "Enable flymake support."
  ;; MG: Some hacking was needed to get this to work. The main
  ;; issue here being that flymake largely seems written with the assumption
  ;; of running and highlighting errors exclusively in the `current-buffer`.
  ;; Error state is buffer-local despite the error reporting format taking a buffer
  ;; However, in our case, we would like flymake to be system aware, as well
  ;; as to continue to report errors if, for instance, we are in another buffer,
  ;; (e.g. the REPL)
  (when sly-asdf-enable-flymake
    (flymake-mode 1)
    (setf *sly-asdf--flymake-backend-state* (make-hash-table))
    ;; Hack to supporting highlighting arbitary buffers
    (advice-add 'flymake--highlight-line :around 'sly-asdf--flymake-highlight-around-hook)
    ;; Run the backend
    (sly-asdf--run-flymake-backend)
    (add-hook 'sly-asdf--after-oos-hook 'sly-asdf--run-flymake-backend nil nil)
    (add-hook 'after-change-functions 'sly-asdf--after-change-function nil nil)
    (add-hook 'after-save-hook 'sly-asdf--after-save-function nil nil)
    (add-hook 'sly-event-hooks 'sly-asdf--before-event-function nil nil)
    (run-with-idle-timer 0.5 t #'sly-asdf-show-popup)
    ;; MG: We cannot use `buffer-list-update-hook` as it is run before the buffer's underlying
    ;; file is visited (buffer-file-name) return nil. Use `post-command-hook` instead?.
    (add-hook 'window-configuration-change-hook 'sly-asdf--flymake-buffer-list-update-hook)
    (run-with-idle-timer 0.5 t #'sly-asdf-show-popup)))


(defvar sly-asdf--buffer-to-system (make-hash-table))
(defvar sly-asdf--system-to-buffers (make-hash-table))
(defvar sly-asdf--last-lisp-buffers nil)

(defun sly-asdf--flymake-buffer-list-update-hook ()
  (let* ((current-buffers (sly-asdf--current-lisp-buffers))
         (new-buffers (cl-set-difference current-buffers sly-asdf--last-lisp-buffers))
         (removed-buffers (cl-set-difference sly-asdf--last-lisp-buffers current-buffers)))
    ;; Update a map of buffers and their associated systems
    (setf sly-asdf--last-lisp-buffers current-buffers)
    ;; For each new buffer, find it's corresponding system. Lisp buffers that do not
    ;; correspond to a system are marked as belonging to "orphan"
    (cl-loop for buffer in new-buffers
             for name = (buffer-name buffer)
             for hash-value = (gethash name sly-asdf--buffer-to-system)
             unless hash-value do
             (setf (gethash name sly-asdf--buffer-to-system)
                   (or (sly-asdf-find-current-system buffer) "orphan")))
    (cl-loop for buffer in removed-buffers
             do (remhash buffer sly-asdf--buffer-to-system))
    (setf sly-asdf--system-to-buffers
          (sly-asdf--buffers-by-system))))


(defun sly-asdf--buffers-by-system ()
  "Get a plist of current asdf systems and their associated buffers.  "
  (interactive)
  (let ((buffers-by-system (make-hash-table :test #'equal)))
    (cl-loop for buffer being the hash-keys of sly-asdf--buffer-to-system
             using (hash-value system) do 
             (push buffer (gethash system buffers-by-system)))
    (message "%s" buffers-by-system)
    buffers-by-system))


(defun sly-asdf--after-change-function (_start _stop _len)
  "Start syntax check for current buffer if it isn't already running.")
;; MG: TODO - For this, we need to send unsaved buffers for compilation
;;(when (sly-asdf--lisp-buffer-p (current-buffer))
;;(sly-asdf--run-flymake-backend)))


(defun sly-asdf--before-event-function (&rest args)
  "Fired before each sly event.  Takes an ARGS, the first value of which is the operation."
  (cl-destructuring-bind ((op &rest)) args
    (when (eq op :channel-send)
      ;;We may have evaluated some expression that may change
      ;; the result of compilation, e.g., loading some other system
      (sly-asdf--run-flymake-backend)))
  nil)


(defun sly-asdf--after-save-function ()
  "Start syntax check for current buffer if it isn't already running."
  (sly-asdf--run-flymake-backend))


(defun sly-asdf--run-flymake-backend ()
  "Flymake backend."
  ;;Override backend-state
  (let ((flymake--backend-state *sly-asdf--flymake-backend-state*))
    (flymake--with-backend-state 'sly-asdf-flymake state
      (let ((run-token (cl-gensym "backend-token")))
        (setf (flymake--backend-state-running state) run-token
              (flymake--backend-state-disabled state) nil
              (flymake--backend-state-diags state) nil
              (flymake--backend-state-reported-p state) nil)
        (funcall 'sly-asdf-flymake
                 (lambda (&rest args)
                   ;;MG: Override backend-state, lexical scoping doesn't work here -> bc async?
                   (let ((flymake--backend-state *sly-asdf--flymake-backend-state*))
                     (message "remove hl")
                     (sly-asdf--remove-highlight-all-buffers)
                     (message "handle report")
                     (apply #'sly-asdf-flymake--handle-report 'sly-asdf-flymake run-token args)
                     )
                   ;; MG: Override backend-state, lexical scoping doesn't work here -> bc async?
                   ))))))


(cl-defun sly-asdf-flymake--handle-report (backend token report-action
                                          &key explanation force
                                          &allow-other-keys)
  "Handle reports from BACKEND identified by TOKEN.
BACKEND, REPORT-ACTION and EXPLANATION, and FORCE conform to the calling
convention described in `flymake-diagnostic-functions' (which
see). Optional FORCE says to handle a report even if TOKEN was
not expected."
  (let* ((state (gethash backend flymake--backend-state))
         (first-report (not (flymake--backend-state-reported-p state))))
    (setf (flymake--backend-state-reported-p state) t)
    (let (expected-token
          new-diags)
      (cond
       ((null state)
        (flymake-error
         "Unexpected report from unknown backend %s" backend))
       ((flymake--backend-state-disabled state)
        (flymake-error
         "Unexpected report from disabled backend %s" backend))
       ((progn
          (setq expected-token (flymake--backend-state-running state))
          (null expected-token))
        ;; should never happen
        (flymake-error "Unexpected report from stopped backend %s" backend))
       ((not (or (eq expected-token token)
                 force))
        (flymake-error "Obsolete report from backend %s with explanation %s"
                       backend explanation))
       ((eq :panic report-action)
        (flymake--disable-backend backend explanation))
       ((not (listp report-action))
        (flymake--disable-backend backend
                                  (format "Unknown action %S" report-action))
        (flymake-error "Expected report, but got unknown key %s" report-action))
       (t
        (setq new-diags report-action)
        (message "new diags: %s" new-diags)
        (save-restriction
          (widen)
          ;; only delete overlays if this is the first report
          ;; (when first-report
          ;;   (flymake-delete-own-overlays
          ;;    (lambda (ov)
          ;;      (eq backend
          ;;          (flymake--diag-backend
          ;;           (overlay-get ov 'flymake-diagnostic)))))

          ;;   )
          (mapc (lambda (diag)
                  (flymake--highlight-line diag)
                  (setf (flymake--diag-backend diag) backend))
                new-diags)
          (setf (flymake--backend-state-diags state)
                (append new-diags (flymake--backend-state-diags state)))
          (when flymake-check-start-time
            (flymake-log :debug "backend %s reported %d diagnostics in %.2f second(s)"
                         backend
                         (length new-diags)
                         (- (float-time) flymake-check-start-time)))
          (when (and (get-buffer (flymake--diagnostics-buffer-name))
                     (get-buffer-window (flymake--diagnostics-buffer-name))
                     (null (cl-set-difference (flymake-running-backends)
                                              (flymake-reporting-backends))))
            (flymake-show-diagnostics-buffer))))))))



(cl-defun sly-asdf-flymake (report-cb &rest _args)
  "Flymake diagnostic function for sly-asdf.  REPORT-FN required callback for flymake."
  (let ((systems (hash-table-keys (sly-asdf--buffers-by-system))))
    ;; Compile each system for which there exists a corresponding buffer
    (cl-loop for system in systems
             if (string-equal system "orphan") do
             ;; Orphaned buffers are compiled separately
             (mapc #'(lambda (buf) (sly-asdf--compile-buffer-for-flymake buf report-cb))
                     (gethash systems system))
             else do
             (sly-asdf--compile-system-for-flymake system report-cb))))


(defun sly-asdf--compile-buffer-for-flymake (buffer report-cb)
  (sly-asdf--compile-for-flymake
   (buffer-file-name buffer)
   (create-flymake-report-fn report-cb (list buffer))))


(defun sly-asdf--compile-system-for-flymake (system report-cb)
  (sly-eval-async `(slynk-asdf:compile-system-for-flymake ,system)
    (create-flymake-report-fn report-cb (gethash system sly-asdf--system-to-buffers))))


(defun create-flymake-report-fn (report-cb buffers)
  (cl-flet ((remove-nulls (list) (remove-if-not #'identity list)))
    (lambda (result)
      (sly-asdf--remove-highlight-from-buffers buffers)
      (if result
          (funcall report-cb
                   (remove-nulls
                    (mapcar (lambda (note)
                              (sly-asdf-note-to-diagnostic note))
                            (sly-compilation-result.notes result)))
                   :force t)
        (funcall report-cb nil :force t)))))


(defun sly-asdf-note-to-diagnostic (note)
  "Create a diagnostic for the given sly NOTE found in the buffer SOURCE."
  (message "%s" (sly-note.location note))
  (let* ((message (sly-note.message note))
         (location (sly-note.location note))
         (severity (sly-note.severity note)))
    ;; Location may be an (:error) w/o a buffer, in which sly-location.buffer
    ;; returns a string (the error message)
    (if (and location (listp (sly-location.buffer location))) 
        (progn 
          (message "location!: %s buf: %s " location (sly-location.buffer location))
        (let ((buffer (get-file-buffer (cadr (sly-location.buffer location))))
              (pos (cadr (sly-location.position location ))))
          (message "buf2!: %s " buffer)
          (when buffer 
            (with-current-buffer buffer
            (save-excursion
              (goto-char pos)
              (let ((bounds (or (sly-bounds-of-symbol-at-point)
                                (sly-bounds-of-sexp-at-point))))
                (if bounds
                    (cl-destructuring-bind (start . end) bounds
                      (flymake-make-diagnostic buffer start end severity message))
                  (flymake-make-diagnostic buffer pos (+ pos 1) severity message))))))))
      (progn
        (message "%s" (car (sly-asdf--current-lisp-buffers)) )
        (flymake-make-diagnostic (car (sly-asdf--current-lisp-buffers)) 1
                                 (buffer-size (car (sly-asdf--current-lisp-buffers))) severity message)))))


(defun sly-asdf--remove-highlight (buffer)
  "Remove flymake overlays from target BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (flymake-delete-own-overlays))))


(defun sly-asdf--remove-highlight-all-buffers ()
  "Remove flymake overlays from all Lisp buffers."
  (cl-mapcar 'sly-asdf--remove-highlight (sly-asdf--current-lisp-buffers)))


;; (cl-defun sly-asdf-flymake (report-fn &rest _args)
;;   "Flymake diagnostic function for sly-asdf.  REPORT-FN required callback for flymake."
;;   (cl-flet ((compile-buffer-for-flycheck (buffer)
;;                                          (sly-asdf-compile-for-flymake
;;                                           (buffer-file-name buffer)
;;                                           (lambda (result)
;;                                             (if result
;;                                                 (funcall report-fn
;;                                                          (mapcar (lambda (note)
;;                                                                    (sly-asdf-note-to-diagnostic note buffer))
;;                                                                  (sly-compilation-result.notes result)))
;;                                               (funcall report-fn nil))))))
;;     (mapcar #'compile-buffer-for-flycheck (sly-asdf--current-lisp-buffers))))


;; (defun sly-asdf-note-to-diagnostic (note source)
;;   "Create a diagnostic for the given sly NOTE found in the buffer SOURCE."
;;   (let ((message (sly-note.message note))
;;         (pos (cadr (sly-location.position (sly-note.location note)))))
;;     (with-current-buffer source
;;       (save-excursion
;;       (goto-char pos)
;;       (let ((bounds (or (sly-bounds-of-symbol-at-point)
;;                         (sly-bounds-of-sexp-at-point))))
;;         (if bounds
;;             (cl-destructuring-bind (start . end) bounds
;;               (flymake-make-diagnostic source start end :error message))
;;           (flymake-make-diagnostic source pos (+ pos 1) :error message)))))))


(defun sly-asdf--remove-highlight-from-buffers (buffers)
  "Remove flymake overlays from all Lisp buffers."
  (cl-mapcar 'sly-asdf--remove-highlight buffers))


(defun sly-asdf--flymake-highlight-around-hook (fun &rest args)
  "Hook to apply around flymake-highlight.
FUN is the original function and ARGS is a list containing
the diagnostic to highlight.  Needed because flymake-highlight does
not pass the diagnostic's buffer to `make-overlay`."
  (let ((diagnostic (car args)))
    (with-current-buffer (flymake--diag-buffer diagnostic)
      (message "buf %s" (current-buffer))
      (apply fun args))))


(defun sly-asdf--lisp-buffer-p (buffer)
  "Check whether BUFFER refers to a Lisp buffer."
  (member (file-name-extension (buffer-name buffer)) *sly-asdf-lisp-extensions*))


(defun sly-asdf--current-lisp-buffers ()
  "Traverses the current `buffer-list`, returning those buffers with a .lisp extension."
  (cl-remove-if-not #'sly-asdf--lisp-buffer-p (buffer-list)))


(defun sly-asdf--compile-for-flymake (filename callback)
  "Compile FILENAME for Emacs, calling CALLBACK with the result of compilation."
  (sly-eval-async `(slynk-asdf:compile-file-for-flymake ,filename) callback))


(defun sly-asdf-show-popup ()
  "Display a popup containing the diagnostic message at the current point."
  (let ((point (point)))
    (unless (equal point *sly-asdf--last-point*)
      (setf *sly-asdf--last-point* point)
      (let ((diags (flymake-diagnostics point)))
        (when diags
          (popup-tip (flymake-diagnostic-text (car diags)) :point point))))))


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
        (sly-compilation-finished result (current-buffer))
        (run-hooks 'sly-asdf--after-oos-hook))))

(defun sly-asdf-compile-system (&optional system)
  "Compile and load an ASDF SYSTEM.
Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (sly-asdf-read-system-name)))
  (sly-asdf-oos system 'compile-op))

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


(cl-defun sly-asdf-find-current-system (&optional (buffer (car (sly-asdf--current-lisp-buffers))))
  "Find the name of the current asd system."
  (when buffer
    (let* ((buffer-file-name (buffer-file-name buffer ))
           (directory (file-name-directory buffer-file-name))
           (system-file (sly-asdf-find-system-file directory)))
      (when system-file
        (file-name-base system-file)))))


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
        (sly-compilation-finished result (current-buffer))
        (run-hooks 'sly-asdf--after-oos-hook))))


;;;###autoload
(add-to-list 'sly-contribs 'sly-asdf 'append)


(provide 'sly-asdf)
;;; sly-asdf.el ends here
