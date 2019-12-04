;;; sly-asdf-flymake.el --- Flymake support for sly-asdf -*- lexical-binding: t; -*-

(require 'flymake)
(require 'popup)
(require 'cl-lib)

(defvar sly-asdf-enable-flymake 1)

(defvar *sly-asdf-lisp-extensions* (list "lisp")
  "File extensions to look for when finding open Lisp files.")

(defvar sly-asdf--buffer-to-system (make-hash-table))
(defvar sly-asdf--system-to-buffers (make-hash-table))
(defvar sly-asdf--last-lisp-buffers nil)
(defvar *sly-asdf--last-point* nil)
(defvar *sly-asdf--flymake-backend-state* nil)
(defvar *sly-asdf--clobber-errors-with-bad-location* nil)
(defvar sly-asdf--after-oos-hook nil)


(cl-defun sly-asdf-find-current-system (&optional (buffer (car (sly-asdf--current-lisp-buffers))))
  "Find the name of the current asd system."
  (when buffer
    (let* ((buffer-file-name (buffer-file-name buffer ))
           (directory (file-name-directory buffer-file-name))
           (system-file (sly-asdf-find-system-file directory)))
      (when system-file
        (file-name-base system-file)))))


(defun sly-asdf-flymake ()
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
    (add-hook 'sly-asdf--after-oos-hook 'sly-asdf--run-flymake-backend nil nil)
    (add-hook 'after-change-functions 'sly-asdf--after-change-function nil nil)
    (add-hook 'after-save-hook 'sly-asdf--after-save-function nil nil)
    ;(add-hook 'sly-event-hooks 'sly-asdf--before-event-function nil nil)
    (run-with-idle-timer 0.5 t #'sly-asdf-show-popup)
    ;; MG: We cannot use `buffer-list-update-hook` as it is run before the buffer's underlying
    ;; file is visited (buffer-file-name) return nil. Use `post-command-hook` instead?.
    (add-hook 'window-configuration-change-hook 'sly-asdf--flymake-buffer-list-update-hook)
    (run-with-idle-timer 0.5 t #'sly-asdf-show-popup)
    ;; Initial setup
    ;; Remove any previous flymake related fasls
    (sly-asdf-remove-flymake-fasls)
    ;; Determine current systems and buffers
    (sly-asdf--flymake-buffer-list-update-hook)
    ;; Run the backend
    (sly-asdf--run-flymake-backend)))


(defun sly-asdf-remove-flymake-fasls ()
  (interactive)
  (sly-eval
   `(slynk-asdf:remove-flymake-fasls)))


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
    buffers-by-system))


(defun sly-asdf--after-change-function (_start _stop _len)
  "Start syntax check for current buffer if it isn't already running.")
;; MG: TODO - For this, we need to send unsaved buffers for compilation
;;(when (sly-asdf--lisp-buffer-p (current-buffer))
;;(sly-asdf--run-flymake-backend)))


;; MG: Let's not do this for now, flychecking the current image has side-effects
;; and is somewhat of a strange experience
;; (defun sly-asdf--before-event-function (&rest args)
;;   "Fired before each sly event.  Takes an ARGS, the first value of which is the operation."
;;   (cl-destructuring-bind ((op &rest)) args
;;     (when (eq op :channel-send)
;;       ;;We may have evaluated some expression that may change
;;       ;; the result of compilation, e.g., loading some other system
;;       (sly-asdf--run-flymake-backend)))
;;   nil)


(defun sly-asdf--after-save-function ()
  "Start syntax check for current buffer if it isn't already running."
  (sly-asdf--run-flymake-backend))


(defun sly-asdf--run-flymake-backend ()
  "Flymake backend."
  ;;Override backend-state
  (let ((flymake--backend-state *sly-asdf--flymake-backend-state*))
    (flymake--with-backend-state 'sly-asdf-flymake-backend state
      (let ((run-token (cl-gensym "backend-token")))
        (setf (flymake--backend-state-running state) run-token
              (flymake--backend-state-disabled state) nil
              (flymake--backend-state-diags state) nil
              (flymake--backend-state-reported-p state) nil)
        (funcall 'sly-asdf-flymake-backend
                 (lambda (&rest args)
                   ;;MG: Override backend-state, lexical scoping doesn't work here -> bc async?
                   (let ((flymake--backend-state *sly-asdf--flymake-backend-state*))
                     (sly-asdf--remove-highlight-all-buffers)
                     (apply #'sly-asdf-flymake--handle-report 'sly-asdf-flymake-backend run-token args)
                     )))))))


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


(cl-defun sly-asdf-flymake-backend (report-cb &rest _args)
  "Flymake diagnostic function for sly-asdf.  REPORT-FN required callback for flymake."
  (let ((systems (hash-table-keys (sly-asdf--buffers-by-system))))
    ;; Compile each system for which there exists a corresponding buffer
    (cl-loop for system in systems
             if (string-equal system "orphan") do
             ;; Orphaned buffers are compiled separately
             (sly-asdf--compile-files-for-flymake (gethash system sly-asdf--system-to-buffers) report-cb)
             else do
             (sly-asdf--compile-system-for-flymake system report-cb))))


(defun sly-asdf--compile-files-for-flymake (filenames report-cb)
  "Compile FILENAMES for Emacs, calling REPORT-CB with the result of compilation."
  (sly-eval-async `(slynk-asdf:compile-files-for-flymake '(,@filenames))
    (create-flymake-report-fn report-cb filenames)))


(defun sly-asdf--compile-system-for-flymake (system report-cb)
  (let ((buffers (gethash system sly-asdf--system-to-buffers)))
    (sly-eval-async `(slynk-asdf:compile-system-for-flymake ,system '(,@buffers))
      (create-flymake-report-fn report-cb buffers))))


(defun create-flymake-report-fn (report-cb buffers)
  (cl-flet ((remove-nulls (list) (cl-remove-if-not #'identity list)))
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



(defun sly-asdf--parse-severity (severity)
  (cl-case severity
    ((:warning :style-warning) :warning)
    (t :error)))


(defun sly-asdf-note-to-diagnostic (note)
  "Create a diagnostic for the given sly NOTE found in the buffer SOURCE."
  (let* ((message (sly-note.message note))
         (location (sly-note.location note))
         (severity (sly-asdf--parse-severity (sly-note.severity note))))
    ;; Location may be an (:error) w/o a buffer, in which sly-location.buffer
    ;; returns a string (the error message)
    (if (and location (listp (sly-location.buffer location))) 
        (progn 
          (let ((buffer (get-file-buffer (cadr (sly-location.buffer location))))
                (pos (cadr (sly-location.position location ))))
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
      (unless *sly-asdf--clobber-errors-with-bad-location*
        (progn
          (flymake-make-diagnostic (car (sly-asdf--current-lisp-buffers)) 1
                                   (buffer-size (car (sly-asdf--current-lisp-buffers))) severity message))))))


(defun sly-asdf--remove-highlight (buffer)
  "Remove flymake overlays from target BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (flymake-delete-own-overlays))))


(defun sly-asdf--remove-highlight-all-buffers ()
  "Remove flymake overlays from all Lisp buffers."
  (cl-mapcar 'sly-asdf--remove-highlight (sly-asdf--current-lisp-buffers)))


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
      (apply fun args))))


(defun sly-asdf-show-popup ()
  "Display a popup containing the diagnostic message at the current point."
  (let ((point (point)))
    (unless (equal point *sly-asdf--last-point*)
      (setf *sly-asdf--last-point* point)
      (let ((diags (flymake-diagnostics point)))
        (when diags
          (popup-tip (flymake-diagnostic-text (car diags)) :point point))))))



(defun sly-asdf--lisp-buffer-p (buffer)
  "Check whether BUFFER refers to a Lisp buffer."
  (member (file-name-extension (buffer-name buffer)) *sly-asdf-lisp-extensions*))


(defun sly-asdf--current-lisp-buffers ()
  "Traverses the current `buffer-list`, returning those buffers with a .lisp extension."
  (cl-remove-if-not #'sly-asdf--lisp-buffer-p (buffer-list)))


(provide 'sly-asdf-flymake)
