;;; sly-asdf-flymake-base.el --- Clone of sly-asdf-flymake-base.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2019 Free Software Foundation, Inc.

;; Author:  Pavel Kobyakov <pk_at_work@yahoo.com>
;; Maintainer: Leo Liu <sdl.web@gmail.com>
;; Version: 0.3
;; Keywords: c languages tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Sly-Asdf-Flymake-Base is a minor Emacs mode performing on-the-fly syntax checks.
;;
;; Sly-Asdf-Flymake-Base collects diagnostic information for multiple sources,
;; called backends, and visually annotates the relevant portions in
;; the buffer.
;;
;; This file contains the UI for displaying and interacting with the
;; results produced by these backends, as well as entry points for
;; backends to hook on to.
;;
;; The main entry points are `sly-asdf-flymake-base-mode' and `sly-asdf-flymake-base-start'
;;
;; The docstrings of these variables are relevant to understanding how
;; Sly-Asdf-Flymake-Base works for both the user and the backend programmer:
;;
;; * `sly-asdf-flymake-base-diagnostic-functions'
;; * `sly-asdf-flymake-base-diagnostic-types-alist'
;;
;; Forked from sly-asdf-flymake-base.el due to number of overrides required for sly-asdf-sly-asdf-flymake-base
;;
;;; Code:

(require 'cl-lib)
(require 'thingatpt) ; end-of-thing
(require 'warnings) ; warning-numeric-level, display-warning
(require 'compile) ; for some faces
;; We need the next require to avoid compiler warnings and run-time
;; errors about mouse-wheel-up/down-event in builds --without-x, where
;; mwheel is not preloaded.
(require 'mwheel)
;; when-let*, if-let*, hash-table-keys, hash-table-values:
(eval-when-compile (require 'subr-x))

(defgroup sly-asdf-flymake-base nil
  "Universal on-the-fly syntax checker."
  :version "23.1"
  :link '(custom-manual "(sly-asdf-flymake-base) Top")
  :group 'tools)

(defcustom sly-asdf-flymake-base-error-bitmap '(sly-asdf-flymake-base-double-exclamation-mark
                                  compilation-error)
  "Bitmap (a symbol) used in the fringe for indicating errors.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `sly-asdf-flymake-base-warning-bitmap'.

The option `sly-asdf-flymake-base-fringe-indicator-position' controls how and where
this is used."
  :version "24.3"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom sly-asdf-flymake-base-warning-bitmap '(exclamation-mark compilation-warning)
  "Bitmap (a symbol) used in the fringe for indicating warnings.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `sly-asdf-flymake-base-error-bitmap'.

The option `sly-asdf-flymake-base-fringe-indicator-position' controls how and where
this is used."
  :version "24.3"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom sly-asdf-flymake-base-note-bitmap '(exclamation-mark compilation-info)
  "Bitmap (a symbol) used in the fringe for indicating info notes.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `sly-asdf-flymake-base-error-bitmap'.

The option `sly-asdf-flymake-base-fringe-indicator-position' controls how and where
this is used."
  :version "26.1"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom sly-asdf-flymake-base-fringe-indicator-position 'left-fringe
  "The position to put Sly-Asdf-Flymake-Base fringe indicator.
The value can be nil (do not use indicators), `left-fringe' or `right-fringe'.
See `sly-asdf-flymake-base-error-bitmap' and `sly-asdf-flymake-base-warning-bitmap'."
  :version "24.3"
  :type '(choice (const left-fringe)
		 (const right-fringe)
		 (const :tag "No fringe indicators" nil)))

(defcustom sly-asdf-flymake-base-start-syntax-check-on-newline t
  "Start syntax check if newline char was added/removed from the buffer."
  :type 'boolean)

(defcustom sly-asdf-flymake-base-no-changes-timeout 0.5
  "Time to wait after last change before automatically checking buffer.
If nil, never start checking buffer automatically like this."
  :type 'number)

(defcustom sly-asdf-flymake-base-gui-warnings-enabled t
  "Enables/disables GUI warnings."
  :type 'boolean)
(make-obsolete-variable 'sly-asdf-flymake-base-gui-warnings-enabled
			"it no longer has any effect." "26.1")

(define-obsolete-variable-alias 'sly-asdf-flymake-base-start-syntax-check-on-find-file
  'sly-asdf-flymake-base-start-on-sly-asdf-flymake-base-mode "26.1")

(defcustom sly-asdf-flymake-base-start-on-sly-asdf-flymake-base-mode t
  "Start syntax check when `sly-asdf-flymake-base-mode' is enabled.
Specifically, start it when the buffer is actually displayed."
  :version "26.1"
  :type 'boolean)

(defcustom sly-asdf-flymake-base-log-level -1
  "Obsolete and ignored variable."
  :type 'integer)
(make-obsolete-variable 'sly-asdf-flymake-base-log-level
			"it is superseded by `warning-minimum-log-level.'"
                        "26.1")

(defcustom sly-asdf-flymake-base-wrap-around t
  "If non-nil, moving to errors wraps around buffer boundaries."
  :version "26.1"
  :type 'boolean)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'sly-asdf-flymake-base-double-exclamation-mark
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b01100110
            #b01100110
            #b01100110
            #b01100110
            #b01100110
            #b01100110
            #b01100110
            #b01100110
            #b00000000
            #b01100110
            #b00000000
            #b00000000
            #b00000000)))

(defvar-local sly-asdf-flymake-base-timer nil
  "Timer for starting syntax check.")

(defvar-local sly-asdf-flymake-base-check-start-time nil
  "Time at which syntax check was started.")

(defun sly-asdf-flymake-base--log-1 (level sublog msg &rest args)
  "Do actual work for `sly-asdf-flymake-base-log'."
  (let (;; never popup the log buffer
        (warning-minimum-level :emergency)
        (warning-type-format
         (format " [%s %s]"
                 (or sublog 'sly-asdf-flymake-base)
                 (current-buffer))))
    (display-warning (list 'sly-asdf-flymake-base sublog)
                     (apply #'format-message msg args)
                     (if (numberp level)
                         (or (nth level
                                  '(:emergency :error :warning :debug :debug) )
                             :error)
                       level)
                     "*Sly-Asdf-Flymake-Base log*")))

(defun sly-asdf-flymake-base-switch-to-log-buffer ()
  "Go to the *Sly-Asdf-Flymake-Base log* buffer."
  (interactive)
  (switch-to-buffer "*Sly-Asdf-Flymake-Base log*"))

;;;###autoload
(defmacro sly-asdf-flymake-base-log (level msg &rest args)
  "Log, at level LEVEL, the message MSG formatted with ARGS.
LEVEL is passed to `display-warning', which is used to display
the warning.  If this form is included in a byte-compiled file,
the generated warning contains an indication of the file that
generated it."
  (let* ((compile-file (and (boundp 'byte-compile-current-file)
                            (symbol-value 'byte-compile-current-file)))
         (sublog (if (and
                      compile-file
                      (not load-file-name))
                     (intern
                      (file-name-nondirectory
                       (file-name-sans-extension compile-file))))))
    `(sly-asdf-flymake-base--log-1 ,level ',sublog ,msg ,@args)))

(defun sly-asdf-flymake-base-error (text &rest args)
  "Format TEXT with ARGS and signal an error for Sly-Asdf-Flymake-Base."
  (let ((msg (apply #'format-message text args)))
    (sly-asdf-flymake-base-log :error msg)
    (error (concat "[Sly-Asdf-Flymake-Base] " msg))))

(cl-defstruct (sly-asdf-flymake-base--diag
               (:constructor sly-asdf-flymake-base--diag-make))
  buffer beg end type text backend)

;;;###autoload
(defun sly-asdf-flymake-base-make-diagnostic (buffer
                                beg
                                end
                                type
                                text)
  "Make a Sly-Asdf-Flymake-Base diagnostic for BUFFER's region from BEG to END.
TYPE is a key to `sly-asdf-flymake-base-diagnostic-types-alist' and TEXT is a
description of the problem detected in this region."
  (sly-asdf-flymake-base--diag-make :buffer buffer :beg beg :end end :type type :text text))

;;;###autoload
(defun sly-asdf-flymake-base-diagnostics (&optional beg end)
  "Get Sly-Asdf-Flymake-Base diagnostics in region determined by BEG and END.

If neither BEG or END is supplied, use the whole buffer,
otherwise if BEG is non-nil and END is nil, consider only
diagnostics at BEG."
  (mapcar (lambda (ov) (overlay-get ov 'sly-asdf-flymake-base-diagnostic))
          (sly-asdf-flymake-base--overlays :beg beg :end end)))

(defmacro sly-asdf-flymake-base--diag-accessor (public internal thing)
  "Make PUBLIC an alias for INTERNAL, add doc using THING."
  `(defsubst ,public (diag)
     ,(format "Get Sly-Asdf-Flymake-Base diagnostic DIAG's %s." (symbol-name thing))
     (,internal diag)))

(sly-asdf-flymake-base--diag-accessor sly-asdf-flymake-base-diagnostic-buffer sly-asdf-flymake-base--diag-buffer buffer)
(sly-asdf-flymake-base--diag-accessor sly-asdf-flymake-base-diagnostic-text sly-asdf-flymake-base--diag-text text)
(sly-asdf-flymake-base--diag-accessor sly-asdf-flymake-base-diagnostic-type sly-asdf-flymake-base--diag-type type)
(sly-asdf-flymake-base--diag-accessor sly-asdf-flymake-base-diagnostic-beg sly-asdf-flymake-base--diag-beg beg)
(sly-asdf-flymake-base--diag-accessor sly-asdf-flymake-base-diagnostic-end sly-asdf-flymake-base--diag-end end)
(sly-asdf-flymake-base--diag-accessor sly-asdf-flymake-base-diagnostic-backend sly-asdf-flymake-base--diag-backend backend)

(cl-defun sly-asdf-flymake-base--overlays (&key beg end filter compare key)
  "Get sly-asdf-flymake-base-related overlays.
If BEG is non-nil and END is nil, consider only `overlays-at'
BEG. Otherwise consider `overlays-in' the region comprised by BEG
and END, defaulting to the whole buffer.  Remove all that do not
verify FILTER, a function, and sort them by COMPARE (using KEY)."
  (save-restriction
    (widen)
    (let ((ovs (cl-remove-if-not
                (lambda (ov)
                  (and (overlay-get ov 'sly-asdf-flymake-base-diagnostic)
                       (or (not filter)
                           (funcall filter ov))))
                (if (and beg (null end))
                    (overlays-at beg t)
                  (overlays-in (or beg (point-min))
                               (or end (point-max)))))))
      (if compare
          (cl-sort ovs compare :key (or key
                                        #'identity))
        ovs))))

(defun sly-asdf-flymake-base-delete-own-overlays (&optional filter)
  "Delete all Sly-Asdf-Flymake-Base overlays in BUFFER."
  (mapc #'delete-overlay (sly-asdf-flymake-base--overlays :filter filter)))

(defface sly-asdf-flymake-base-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :inherit error))
  "Face used for marking error regions."
  :version "24.4")

(defface sly-asdf-flymake-base-warning
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "deep sky blue"))
    (t
     :inherit warning))
  "Face used for marking warning regions."
  :version "24.4")

(defface sly-asdf-flymake-base-note
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "yellow green"))
    (t
     :inherit warning))
  "Face used for marking note regions."
  :version "26.1")

(define-obsolete-face-alias 'sly-asdf-flymake-base-warnline 'sly-asdf-flymake-base-warning "26.1")
(define-obsolete-face-alias 'sly-asdf-flymake-base-errline 'sly-asdf-flymake-base-error "26.1")

;;;###autoload
(defun sly-asdf-flymake-base-diag-region (buffer line &optional col)
  "Compute BUFFER's region (BEG . END) corresponding to LINE and COL.
If COL is nil, return a region just for LINE.  Return nil if the
region is invalid."
  (condition-case-unless-debug _err
      (with-current-buffer buffer
        (let ((line (min (max line 1)
                         (line-number-at-pos (point-max) 'absolute))))
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (cl-flet ((fallback-bol
                       ()
                       (back-to-indentation)
                       (if (eobp)
                           (line-beginning-position 0)
                         (point)))
                      (fallback-eol
                       (beg)
                       (progn
                         (end-of-line)
                         (skip-chars-backward " \t\f\t\n" beg)
                         (if (eq (point) beg)
                             (line-beginning-position 2)
                           (point)))))
              (if (and col (cl-plusp col))
                  (let* ((beg (progn (forward-char (1- col))
                                     (point)))
                         (sexp-end (ignore-errors (end-of-thing 'sexp)))
                         (end (or (and sexp-end
                                       (not (= sexp-end beg))
                                       sexp-end)
                                  (and (< (goto-char (1+ beg)) (point-max))
                                       (point)))))
                    (if end
                        (cons beg end)
                      (cons (setq beg (fallback-bol))
                            (fallback-eol beg))))
                (let* ((beg (fallback-bol))
                       (end (fallback-eol beg)))
                  (cons beg end)))))))
    (error (sly-asdf-flymake-base-log :warning "Invalid region line=%s col=%s" line col)
           nil)))

(defvar sly-asdf-flymake-base-diagnostic-functions nil
  "Special hook of Sly-Asdf-Flymake-Base backends that check a buffer.

The functions in this hook diagnose problems in a buffer's
contents and provide information to the Sly-Asdf-Flymake-Base user interface
about where and how to annotate problems diagnosed in a buffer.

Each backend function must be prepared to accept an arbitrary
number of arguments:

* the first argument is always REPORT-FN, a callback function
  detailed below;

* the remaining arguments are keyword-value pairs in the
  form (:KEY VALUE :KEY2 VALUE2...).  Currently, Sly-Asdf-Flymake-Base provides
  no such arguments, but backend functions must be prepared to
  accept and possibly ignore any number of them.

Whenever Sly-Asdf-Flymake-Base or the user decides to re-check the buffer,
backend functions are called as detailed above and are expected
to initiate this check, but aren't required to complete it before
exiting: if the computation involved is expensive, especially for
large buffers, that task can be scheduled for the future using
asynchronous processes or other asynchronous mechanisms.

In any case, backend functions are expected to return quickly or
signal an error, in which case the backend is disabled.  Sly-Asdf-Flymake-Base
will not try disabled backends again for any future checks of
this buffer.  Certain commands, like turning `sly-asdf-flymake-base-mode' off
and on again, reset the list of disabled backends.

If the function returns, Sly-Asdf-Flymake-Base considers the backend to be
\"running\". If it has not done so already, the backend is
expected to call the function REPORT-FN with a single argument
REPORT-ACTION also followed by an optional list of keyword-value
pairs in the form (:REPORT-KEY VALUE :REPORT-KEY2 VALUE2...).

Currently accepted values for REPORT-ACTION are:

* A (possibly empty) list of diagnostic objects created with
  `sly-asdf-flymake-base-make-diagnostic', causing Sly-Asdf-Flymake-Base to annotate the
  buffer with this information.

  A backend may call REPORT-FN repeatedly in this manner, but
  only until Sly-Asdf-Flymake-Base considers that the most recently requested
  buffer check is now obsolete because, say, buffer contents have
  changed in the meantime. The backend is only given notice of
  this via a renewed call to the backend function. Thus, to
  prevent making obsolete reports and wasting resources, backend
  functions should first cancel any ongoing processing from
  previous calls.

* The symbol `:panic', signaling that the backend has encountered
  an exceptional situation and should be disabled.

Currently accepted REPORT-KEY arguments are:

* `:explanation' value should give user-readable details of
  the situation encountered, if any.

* `:force': value should be a boolean suggesting that Sly-Asdf-Flymake-Base
  consider the report even if it was somehow unexpected.")

(defvar sly-asdf-flymake-base-diagnostic-types-alist
  `((:error
     . ((sly-asdf-flymake-base-category . sly-asdf-flymake-base-error)))
    (:warning
     . ((sly-asdf-flymake-base-category . sly-asdf-flymake-base-warning)))
    (:note
     . ((sly-asdf-flymake-base-category . sly-asdf-flymake-base-note))))
  "Alist ((KEY . PROPS)*) of properties of Sly-Asdf-Flymake-Base diagnostic types.
KEY designates a kind of diagnostic can be anything passed as
`:type' to `sly-asdf-flymake-base-make-diagnostic'.

PROPS is an alist of properties that are applied, in order, to
the diagnostics of the type designated by KEY.  The recognized
properties are:

* Every property pertaining to overlays, except `category' and
  `evaporate' (see Info Node `(elisp)Overlay Properties'), used
  to affect the appearance of Sly-Asdf-Flymake-Base annotations.

* `bitmap', an image displayed in the fringe according to
  `sly-asdf-flymake-base-fringe-indicator-position'.  The value actually
  follows the syntax of `sly-asdf-flymake-base-error-bitmap' (which see).  It
  is overridden by any `before-string' overlay property.

* `severity', a non-negative integer specifying the diagnostic's
  severity.  The higher, the more serious.  If the overlay
  property `priority' is not specified, `severity' is used to set
  it and help sort overlapping overlays.

* `sly-asdf-flymake-base-category', a symbol whose property list is considered
  a default for missing values of any other properties.  This is
  useful to backend authors when creating new diagnostic types
  that differ from an existing type by only a few properties.")

(put 'sly-asdf-flymake-base-error 'face 'sly-asdf-flymake-base-error)
(put 'sly-asdf-flymake-base-error 'bitmap 'sly-asdf-flymake-base-error-bitmap)
(put 'sly-asdf-flymake-base-error 'severity (warning-numeric-level :error))
(put 'sly-asdf-flymake-base-error 'mode-line-face 'compilation-error)

(put 'sly-asdf-flymake-base-warning 'face 'sly-asdf-flymake-base-warning)
(put 'sly-asdf-flymake-base-warning 'bitmap 'sly-asdf-flymake-base-warning-bitmap)
(put 'sly-asdf-flymake-base-warning 'severity (warning-numeric-level :warning))
(put 'sly-asdf-flymake-base-warning 'mode-line-face 'compilation-warning)

(put 'sly-asdf-flymake-base-note 'face 'sly-asdf-flymake-base-note)
(put 'sly-asdf-flymake-base-note 'bitmap 'sly-asdf-flymake-base-note-bitmap)
(put 'sly-asdf-flymake-base-note 'severity (warning-numeric-level :debug))
(put 'sly-asdf-flymake-base-note 'mode-line-face 'compilation-info)

(defun sly-asdf-flymake-base--lookup-type-property (type prop &optional default)
  "Look up PROP for TYPE in `sly-asdf-flymake-base-diagnostic-types-alist'.
If TYPE doesn't declare PROP in either
`sly-asdf-flymake-base-diagnostic-types-alist' or in the symbol of its
associated `sly-asdf-flymake-base-category' return DEFAULT."
  (let ((alist-probe (assoc type sly-asdf-flymake-base-diagnostic-types-alist)))
    (cond (alist-probe
           (let* ((alist (cdr alist-probe))
                  (prop-probe (assoc prop alist)))
             (if prop-probe
                 (cdr prop-probe)
               (if-let* ((cat (assoc-default 'sly-asdf-flymake-base-category alist))
                         (plist (and (symbolp cat)
                                     (symbol-plist cat)))
                         (cat-probe (plist-member plist prop)))
                   (cadr cat-probe)
                 default))))
          (t
           default))))

(defun sly-asdf-flymake-base--fringe-overlay-spec (bitmap &optional recursed)
  (if (and (symbolp bitmap)
           (boundp bitmap)
           (not recursed))
      (sly-asdf-flymake-base--fringe-overlay-spec
       (symbol-value bitmap) t)
    (and sly-asdf-flymake-base-fringe-indicator-position
         bitmap
         (propertize "!" 'display
                     (cons sly-asdf-flymake-base-fringe-indicator-position
                           (if (listp bitmap)
                               bitmap
                             (list bitmap)))))))

(defun sly-asdf-flymake-base--highlight-line (diagnostic)
  "Highlight buffer with info in DIAGNOSTIC."
  (when-let* ((ov (make-overlay
                   (sly-asdf-flymake-base--diag-beg diagnostic)
                   (sly-asdf-flymake-base--diag-end diagnostic))))
    ;; First set `category' in the overlay, then copy over every other
    ;; property.
    ;;
    (let ((alist (assoc-default (sly-asdf-flymake-base--diag-type diagnostic)
                                sly-asdf-flymake-base-diagnostic-types-alist)))
      (overlay-put ov 'category (assoc-default 'sly-asdf-flymake-base-category alist))
      (cl-loop for (k . v) in alist
               unless (eq k 'category)
               do (overlay-put ov k v)))
    ;; Now ensure some essential defaults are set
    ;;
    (cl-flet ((default-maybe
                (prop value)
                (unless (or (plist-member (overlay-properties ov) prop)
                            (let ((cat (overlay-get ov
                                                    'sly-asdf-flymake-base-category)))
                              (and cat
                                   (plist-member (symbol-plist cat) prop))))
                  (overlay-put ov prop value))))
      (default-maybe 'bitmap 'sly-asdf-flymake-base-error-bitmap)
      (default-maybe 'face 'sly-asdf-flymake-base-error)
      (default-maybe 'before-string
        (sly-asdf-flymake-base--fringe-overlay-spec
         (overlay-get ov 'bitmap)))
      (default-maybe 'help-echo
        (lambda (window _ov pos)
          (with-selected-window window
            (mapconcat
             #'sly-asdf-flymake-base--diag-text
             (sly-asdf-flymake-base-diagnostics pos)
             "\n"))))
      (default-maybe 'severity (warning-numeric-level :error))
      (default-maybe 'priority (+ 100 (overlay-get ov 'severity))))
    ;; Some properties can't be overridden.
    ;;
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'sly-asdf-flymake-base-diagnostic diagnostic)))

;; Nothing in Sly-Asdf-Flymake-Base uses this at all any more, so this is just for
;; third-party compatibility.
(define-obsolete-function-alias 'sly-asdf-flymake-base-display-warning 'message-box "26.1")

(defvar-local sly-asdf-flymake-base--backend-state nil
  "Buffer-local hash table of a Sly-Asdf-Flymake-Base backend's state.
The keys to this hash table are functions as found in
`sly-asdf-flymake-base-diagnostic-functions'. The values are structures
of the type `sly-asdf-flymake-base--backend-state', with these slots:

`running', a symbol to keep track of a backend's replies via its
REPORT-FN argument. A backend is running if this key is
present. If nil, Sly-Asdf-Flymake-Base isn't expecting any replies from the
backend.

`diags', a (possibly empty) list of recent diagnostic objects
created by the backend with `sly-asdf-flymake-base-make-diagnostic'.

`reported-p', a boolean indicating if the backend has replied
since it last was contacted.

`disabled', a string with the explanation for a previous
exceptional situation reported by the backend, nil if the
backend is operating normally.")

(cl-defstruct (sly-asdf-flymake-base--backend-state
               (:constructor sly-asdf-flymake-base--make-backend-state))
  running reported-p disabled diags)

(defmacro sly-asdf-flymake-base--with-backend-state (backend state-var &rest body)
  "Bind BACKEND's STATE-VAR to its state, run BODY."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  (let ((b (make-symbol "b")))
    `(let* ((,b ,backend)
            (,state-var
             (or (gethash ,b sly-asdf-flymake-base--backend-state)
                 (puthash ,b (sly-asdf-flymake-base--make-backend-state)
                          sly-asdf-flymake-base--backend-state))))
       ,@body)))

(defun sly-asdf-flymake-base-is-running ()
  "Tell if Sly-Asdf-Flymake-Base has running backends in this buffer"
  (sly-asdf-flymake-base-running-backends))

(cl-defun sly-asdf-flymake-base--handle-report (backend token report-action
                                          &key explanation force
                                          &allow-other-keys)
  "Handle reports from BACKEND identified by TOKEN.
BACKEND, REPORT-ACTION and EXPLANATION, and FORCE conform to the calling
convention described in `sly-asdf-flymake-base-diagnostic-functions' (which
see). Optional FORCE says to handle a report even if TOKEN was
not expected."
  (let* ((state (gethash backend sly-asdf-flymake-base--backend-state))
         (first-report (not (sly-asdf-flymake-base--backend-state-reported-p state))))
    (setf (sly-asdf-flymake-base--backend-state-reported-p state) t)
    (let (expected-token
          new-diags)
      (cond
       ((null state)
        (sly-asdf-flymake-base-error
         "Unexpected report from unknown backend %s" backend))
       ((sly-asdf-flymake-base--backend-state-disabled state)
        (sly-asdf-flymake-base-error
         "Unexpected report from disabled backend %s" backend))
       ((progn
          (setq expected-token (sly-asdf-flymake-base--backend-state-running state))
          (null expected-token))
        ;; should never happen
        (sly-asdf-flymake-base-error "Unexpected report from stopped backend %s" backend))
       ((not (or (eq expected-token token)
                 force))
        (sly-asdf-flymake-base-error "Obsolete report from backend %s with explanation %s"
                       backend explanation))
       ((eq :panic report-action)
        (sly-asdf-flymake-base--disable-backend backend explanation))
       ((not (listp report-action))
        (sly-asdf-flymake-base--disable-backend backend
                                  (format "Unknown action %S" report-action))
        (sly-asdf-flymake-base-error "Expected report, but got unknown key %s" report-action))
       (t
        (setq new-diags report-action)
        (save-restriction
          (widen)
          ;; only delete overlays if this is the first report
          (when first-report
            (sly-asdf-flymake-base-delete-own-overlays
             (lambda (ov)
               (eq backend
                   (sly-asdf-flymake-base--diag-backend
                    (overlay-get ov 'sly-asdf-flymake-base-diagnostic))))))
          (mapc (lambda (diag)
                  (sly-asdf-flymake-base--highlight-line diag)
                  (setf (sly-asdf-flymake-base--diag-backend diag) backend))
                new-diags)
          (setf (sly-asdf-flymake-base--backend-state-diags state)
                (append new-diags (sly-asdf-flymake-base--backend-state-diags state)))
          (when sly-asdf-flymake-base-check-start-time
            (sly-asdf-flymake-base-log :debug "backend %s reported %d diagnostics in %.2f second(s)"
                         backend
                         (length new-diags)
                         (- (float-time) sly-asdf-flymake-base-check-start-time)))
          (when (and (get-buffer (sly-asdf-flymake-base--diagnostics-buffer-name))
                     (get-buffer-window (sly-asdf-flymake-base--diagnostics-buffer-name))
                     (null (cl-set-difference (sly-asdf-flymake-base-running-backends)
                                              (sly-asdf-flymake-base-reporting-backends))))
            (sly-asdf-flymake-base-show-diagnostics-buffer))))))))

(defun sly-asdf-flymake-base-make-report-fn (backend &optional token)
  "Make a suitable anonymous report function for BACKEND.
BACKEND is used to help Sly-Asdf-Flymake-Base distinguish different diagnostic
sources.  If provided, TOKEN helps Sly-Asdf-Flymake-Base distinguish between
different runs of the same backend."
  (let ((buffer (current-buffer)))
    (lambda (&rest args)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (apply #'sly-asdf-flymake-base--handle-report backend token args))))))

(defun sly-asdf-flymake-base--collect (fn &optional message-prefix)
  "Collect Sly-Asdf-Flymake-Base backends matching FN.
If MESSAGE-PREFIX, echo a message using that prefix"
  (unless sly-asdf-flymake-base--backend-state
    (user-error "Sly-Asdf-Flymake-Base is not initialized"))
  (let (retval)
    (maphash (lambda (backend state)
               (when (funcall fn state) (push backend retval)))
             sly-asdf-flymake-base--backend-state)
    (when message-prefix
      (message "%s%s"
               message-prefix
               (mapconcat (lambda (s) (format "%s" s))
                          retval ", ")))
    retval))

(defun sly-asdf-flymake-base-running-backends ()
  "Compute running Sly-Asdf-Flymake-Base backends in current buffer."
  (interactive)
  (sly-asdf-flymake-base--collect #'sly-asdf-flymake-base--backend-state-running
                    (and (called-interactively-p 'interactive)
                         "Running backends: ")))

(defun sly-asdf-flymake-base-disabled-backends ()
  "Compute disabled Sly-Asdf-Flymake-Base backends in current buffer."
  (interactive)
  (sly-asdf-flymake-base--collect #'sly-asdf-flymake-base--backend-state-disabled
                    (and (called-interactively-p 'interactive)
                         "Disabled backends: ")))

(defun sly-asdf-flymake-base-reporting-backends ()
  "Compute reporting Sly-Asdf-Flymake-Base backends in current buffer."
  (interactive)
  (sly-asdf-flymake-base--collect #'sly-asdf-flymake-base--backend-state-reported-p
                    (and (called-interactively-p 'interactive)
                         "Reporting backends: ")))

(defun sly-asdf-flymake-base--disable-backend (backend &optional explanation)
  "Disable BACKEND because EXPLANATION.
If it is running also stop it."
  (sly-asdf-flymake-base-log :warning "Disabling backend %s because %s" backend explanation)
  (sly-asdf-flymake-base--with-backend-state backend state
    (setf (sly-asdf-flymake-base--backend-state-running state) nil
          (sly-asdf-flymake-base--backend-state-disabled state) explanation
          (sly-asdf-flymake-base--backend-state-reported-p state) t)))

(defun sly-asdf-flymake-base--run-backend (backend)
  "Run the backend BACKEND, reenabling if necessary."
  (sly-asdf-flymake-base-log :debug "Running backend %s" backend)
  (let ((run-token (cl-gensym "backend-token")))
    (sly-asdf-flymake-base--with-backend-state backend state
      (setf (sly-asdf-flymake-base--backend-state-running state) run-token
            (sly-asdf-flymake-base--backend-state-disabled state) nil
            (sly-asdf-flymake-base--backend-state-diags state) nil
            (sly-asdf-flymake-base--backend-state-reported-p state) nil))
    ;; FIXME: Should use `condition-case-unless-debug' here, but don't
    ;; for two reasons: (1) that won't let me catch errors from inside
    ;; `ert-deftest' where `debug-on-error' appears to be always
    ;; t. (2) In cases where the user is debugging elisp somewhere
    ;; else, and using sly-asdf-flymake-base, the presence of a frequently
    ;; misbehaving backend in the global hook (most likely the legacy
    ;; backend) will trigger an annoying backtrace.
    ;;
    (condition-case err
        (funcall backend
                 (sly-asdf-flymake-base-make-report-fn backend run-token))
      (error
       (sly-asdf-flymake-base--disable-backend backend err)))))

(defun sly-asdf-flymake-base-start (&optional deferred force)
  "Start a syntax check for the current buffer.
DEFERRED is a list of symbols designating conditions to wait for
before actually starting the check.  If it is nil (the list is
empty), start it immediately, else defer the check to when those
conditions are met.  Currently recognized conditions are
`post-command', for waiting until the current command is over,
`on-display', for waiting until the buffer is actually displayed
in a window.  If DEFERRED is t, wait for all known conditions.

With optional FORCE run even disabled backends.

Interactively, with a prefix arg, FORCE is t."
  (interactive (list nil current-prefix-arg))
  (let ((deferred (if (eq t deferred)
                      '(post-command on-display)
                    deferred))
        (buffer (current-buffer)))
    (cl-labels
        ((start-post-command
          ()
          (remove-hook 'post-command-hook #'start-post-command
                       nil)
          ;; The buffer may have disappeared already, e.g. because of
          ;; code like `(with-temp-buffer (python-mode) ...)'.
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (sly-asdf-flymake-base-start (remove 'post-command deferred) force))))
         (start-on-display
          ()
          (remove-hook 'window-configuration-change-hook #'start-on-display
                       'local)
          (sly-asdf-flymake-base-start (remove 'on-display deferred) force)))
      (cond ((and (memq 'post-command deferred)
                  this-command)
             (add-hook 'post-command-hook
                       #'start-post-command
                       'append nil))
            ((and (memq 'on-display deferred)
                  (not (get-buffer-window (current-buffer))))
             (add-hook 'window-configuration-change-hook
                       #'start-on-display
                       'append 'local))
            (t
             (setq sly-asdf-flymake-base-check-start-time (float-time))
             (run-hook-wrapped
              'sly-asdf-flymake-base-diagnostic-functions
              (lambda (backend)
                (cond
                 ((and (not force)
                       (sly-asdf-flymake-base--with-backend-state backend state
                         (sly-asdf-flymake-base--backend-state-disabled state)))
                  (sly-asdf-flymake-base-log :debug "Backend %s is disabled, not starting"
                               backend))
                 (t
                  (sly-asdf-flymake-base--run-backend backend)))
                nil)))))))

(defvar sly-asdf-flymake-base-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Keymap for `sly-asdf-flymake-base-mode'")

;;;###autoload
(define-minor-mode sly-asdf-flymake-base-mode
  "Toggle Sly-Asdf-Flymake-Base mode on or off.
With a prefix argument ARG, enable Sly-Asdf-Flymake-Base mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Sly-Asdf-Flymake-Base is an Emacs minor mode for on-the-fly syntax checking.
Sly-Asdf-Flymake-Base collects diagnostic information from multiple sources,
called backends, and visually annotates the buffer with the
results.

Sly-Asdf-Flymake-Base performs these checks while the user is editing.  The
customization variables `sly-asdf-flymake-base-start-on-sly-asdf-flymake-base-mode',
`sly-asdf-flymake-base-no-changes-timeout' and
`sly-asdf-flymake-base-start-syntax-check-on-newline' determine the exact
circumstances whereupon Sly-Asdf-Flymake-Base decides to initiate a check of
the buffer.

The commands `sly-asdf-flymake-base-goto-next-error' and
`sly-asdf-flymake-base-goto-prev-error' can be used to navigate among Sly-Asdf-Flymake-Base
diagnostics annotated in the buffer.

The visual appearance of each type of diagnostic can be changed
in the variable `sly-asdf-flymake-base-diagnostic-types-alist'.

Activation or deactivation of backends used by Sly-Asdf-Flymake-Base in each
buffer happens via the special hook
`sly-asdf-flymake-base-diagnostic-functions'.

Some backends may take longer than others to respond or complete,
and some may decide to disable themselves if they are not
suitable for the current buffer. The commands
`sly-asdf-flymake-base-running-backends', `sly-asdf-flymake-base-disabled-backends' and
`sly-asdf-flymake-base-reporting-backends' summarize the situation, as does the
special *Sly-Asdf-Flymake-Base log* buffer."  :group 'sly-asdf-flymake-base :lighter
  sly-asdf-flymake-base--mode-line-format :keymap sly-asdf-flymake-base-mode-map
  (cond
   ;; Turning the mode ON.
   (sly-asdf-flymake-base-mode
    (add-hook 'after-change-functions 'sly-asdf-flymake-base-after-change-function nil t)
    (add-hook 'after-save-hook 'sly-asdf-flymake-base-after-save-hook nil t)
    (add-hook 'kill-buffer-hook 'sly-asdf-flymake-base-kill-buffer-hook nil t)

    (setq sly-asdf-flymake-base--backend-state (make-hash-table))

    (when sly-asdf-flymake-base-start-on-sly-asdf-flymake-base-mode (sly-asdf-flymake-base-start t)))

   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'sly-asdf-flymake-base-after-change-function t)
    (remove-hook 'after-save-hook 'sly-asdf-flymake-base-after-save-hook t)
    (remove-hook 'kill-buffer-hook 'sly-asdf-flymake-base-kill-buffer-hook t)
    ;;+(remove-hook 'find-file-hook (function sly-asdf-flymake-base-find-file-hook) t)

    (sly-asdf-flymake-base-delete-own-overlays)

    (when sly-asdf-flymake-base-timer
      (cancel-timer sly-asdf-flymake-base-timer)
      (setq sly-asdf-flymake-base-timer nil)))))

(defun sly-asdf-flymake-base--schedule-timer-maybe ()
  "(Re)schedule an idle timer for checking the buffer.
Do it only if `sly-asdf-flymake-base-no-changes-timeout' is non-nil."
  (when sly-asdf-flymake-base-timer (cancel-timer sly-asdf-flymake-base-timer))
  (when sly-asdf-flymake-base-no-changes-timeout
    (setq
     sly-asdf-flymake-base-timer
     (run-with-idle-timer
      (seconds-to-time sly-asdf-flymake-base-no-changes-timeout)
      nil
      (lambda (buffer)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and sly-asdf-flymake-base-mode
                       sly-asdf-flymake-base-no-changes-timeout)
	      (sly-asdf-flymake-base-log
               :debug "starting syntax check after idle for %s seconds"
               sly-asdf-flymake-base-no-changes-timeout)
	      (sly-asdf-flymake-base-start t))
            (setq sly-asdf-flymake-base-timer nil))))
      (current-buffer)))))

;;;###autoload
(defun sly-asdf-flymake-base-mode-on ()
  "Turn Sly-Asdf-Flymake-Base mode on."
  (sly-asdf-flymake-base-mode 1))

;;;###autoload
(defun sly-asdf-flymake-base-mode-off ()
  "Turn Sly-Asdf-Flymake-Base mode off."
  (sly-asdf-flymake-base-mode 0))

(make-obsolete 'sly-asdf-flymake-base-mode-on 'sly-asdf-flymake-base-mode "26.1")
(make-obsolete 'sly-asdf-flymake-base-mode-off 'sly-asdf-flymake-base-mode "26.1")

(defun sly-asdf-flymake-base-after-change-function (start stop _len)
  "Start syntax check for current buffer if it isn't already running."
  (let((new-text (buffer-substring start stop)))
    (when (and sly-asdf-flymake-base-start-syntax-check-on-newline (equal new-text "\n"))
      (sly-asdf-flymake-base-log :debug "starting syntax check as new-line has been seen")
      (sly-asdf-flymake-base-start t))
    (sly-asdf-flymake-base--schedule-timer-maybe)))

(defun sly-asdf-flymake-base-after-save-hook ()
  (when sly-asdf-flymake-base-mode
    (sly-asdf-flymake-base-log :debug "starting syntax check as buffer was saved")
    (sly-asdf-flymake-base-start t)))

(defun sly-asdf-flymake-base-kill-buffer-hook ()
  (when sly-asdf-flymake-base-timer
    (cancel-timer sly-asdf-flymake-base-timer)
    (setq sly-asdf-flymake-base-timer nil)))

(defun sly-asdf-flymake-base-find-file-hook ()
  (unless (or sly-asdf-flymake-base-mode
              (null sly-asdf-flymake-base-diagnostic-functions))
    (sly-asdf-flymake-base-mode)
    (sly-asdf-flymake-base-log :warning "Turned on in `sly-asdf-flymake-base-find-file-hook'")))

(defun sly-asdf-flymake-base-goto-next-error (&optional n filter interactive)
  "Go to Nth next Sly-Asdf-Flymake-Base diagnostic that matches FILTER.
Interactively, always move to the next diagnostic.  With a prefix
arg, skip any diagnostics with a severity less than `:warning'.

If `sly-asdf-flymake-base-wrap-around' is non-nil and no more next diagnostics,
resumes search from top.

FILTER is a list of diagnostic types found in
`sly-asdf-flymake-base-diagnostic-types-alist', or nil, if no filter is to be
applied."
  ;; TODO: let filter be a number, a severity below which diags are
  ;; skipped.
  (interactive (list 1
                     (if current-prefix-arg
                         '(:error :warning))
                     t))
  (let* ((n (or n 1))
         (ovs (sly-asdf-flymake-base--overlays :filter
                                 (lambda (ov)
                                   (let ((diag (overlay-get
                                                ov
                                                'sly-asdf-flymake-base-diagnostic)))
                                     (and diag
                                          (or (not filter)
                                              (memq (sly-asdf-flymake-base--diag-type diag)
                                                    filter)))))
                                 :compare (if (cl-plusp n) #'< #'>)
                                 :key #'overlay-start))
         (tail (cl-member-if (lambda (ov)
                               (if (cl-plusp n)
                                   (> (overlay-start ov)
                                      (point))
                                 (< (overlay-start ov)
                                    (point))))
                             ovs))
         (chain (if sly-asdf-flymake-base-wrap-around
                    (if tail
                        (progn (setcdr (last tail) ovs) tail)
                      (and ovs (setcdr (last ovs) ovs)))
                  tail))
         (target (nth (1- n) chain)))
    (cond (target
           (goto-char (overlay-start target))
           (when interactive
             (message
              "%s"
              (funcall (overlay-get target 'help-echo)
                       (selected-window) target (point)))))
          (interactive
           (user-error "No more Sly-Asdf-Flymake-Base errors%s"
                       (if filter
                           (format " of types %s" filter)
                         ""))))))

(defun sly-asdf-flymake-base-goto-prev-error (&optional n filter interactive)
  "Go to Nth previous Sly-Asdf-Flymake-Base diagnostic that matches FILTER.
Interactively, always move to the previous diagnostic.  With a
prefix arg, skip any diagnostics with a severity less than
`:warning'.

If `sly-asdf-flymake-base-wrap-around' is non-nil and no more previous
diagnostics, resumes search from bottom.

FILTER is a list of diagnostic types found in
`sly-asdf-flymake-base-diagnostic-types-alist', or nil, if no filter is to be
applied."
  (interactive (list 1 (if current-prefix-arg
                           '(:error :warning))
                     t))
  (sly-asdf-flymake-base-goto-next-error (- (or n 1)) filter interactive))


;;; Mode-line and menu
;;;
(easy-menu-define sly-asdf-flymake-base-menu sly-asdf-flymake-base-mode-map "Sly-Asdf-Flymake-Base"
  `("Sly-Asdf-Flymake-Base"
    [ "Go to next problem"      sly-asdf-flymake-base-goto-next-error t ]
    [ "Go to previous problem"  sly-asdf-flymake-base-goto-prev-error t ]
    [ "Check now"               sly-asdf-flymake-base-start t ]
    [ "List all problems"       sly-asdf-flymake-base-show-diagnostics-buffer t ]
    "--"
    [ "Go to log buffer"        sly-asdf-flymake-base-switch-to-log-buffer t ]
    [ "Turn off Sly-Asdf-Flymake-Base"        sly-asdf-flymake-base-mode t ]))

(defvar sly-asdf-flymake-base--mode-line-format `(:eval (sly-asdf-flymake-base--mode-line-format)))

(put 'sly-asdf-flymake-base--mode-line-format 'risky-local-variable t)

(defun sly-asdf-flymake-base--mode-line-format ()
  "Produce a pretty minor mode indicator."
  (let* ((known (hash-table-keys sly-asdf-flymake-base--backend-state))
         (running (sly-asdf-flymake-base-running-backends))
         (disabled (sly-asdf-flymake-base-disabled-backends))
         (reported (sly-asdf-flymake-base-reporting-backends))
         (diags-by-type (make-hash-table))
         (all-disabled (and disabled (null running)))
         (some-waiting (cl-set-difference running reported)))
    (maphash (lambda (_b state)
               (mapc (lambda (diag)
                       (push diag
                             (gethash (sly-asdf-flymake-base--diag-type diag)
                                      diags-by-type)))
                     (sly-asdf-flymake-base--backend-state-diags state)))
             sly-asdf-flymake-base--backend-state)
    `((:propertize " Sly-Asdf-Flymake-Base"
                   mouse-face mode-line-highlight
                   help-echo
                   ,(concat (format "%s known backends\n" (length known))
                            (format "%s running\n" (length running))
                            (format "%s disabled\n" (length disabled))
                            "mouse-1: Display minor mode menu\n"
                            "mouse-2: Show help for minor mode")
                   keymap
                   ,(let ((map (make-sparse-keymap)))
                      (define-key map [mode-line down-mouse-1]
                        sly-asdf-flymake-base-menu)
                      (define-key map [mode-line mouse-2]
                        (lambda ()
                          (interactive)
                          (describe-function 'sly-asdf-flymake-base-mode)))
                      map))
      ,@(pcase-let ((`(,ind ,face ,explain)
                     (cond ((null known)
                            `("?" mode-line "No known backends"))
                           (some-waiting
                            `("Wait" compilation-mode-line-run
                              ,(format "Waiting for %s running backend(s)"
                                       (length some-waiting))))
                           (all-disabled
                            `("!" compilation-mode-line-run
                              "All backends disabled"))
                           (t
                            `(nil nil nil)))))
          (when ind
            `((":"
               (:propertize ,ind
                            face ,face
                            help-echo ,explain
                            keymap
                            ,(let ((map (make-sparse-keymap)))
                               (define-key map [mode-line mouse-1]
                                 'sly-asdf-flymake-base-switch-to-log-buffer)
                               map))))))
      ,@(unless (or all-disabled
                    (null known))
          (cl-loop
           for (type . severity)
           in (cl-sort (mapcar (lambda (type)
                                 (cons type (sly-asdf-flymake-base--lookup-type-property
                                             type
                                             'severity
                                             (warning-numeric-level :error))))
                               (cl-union (hash-table-keys diags-by-type)
                                         '(:error :warning)))
                       #'>
                       :key #'cdr)
           for diags = (gethash type diags-by-type)
           for face = (sly-asdf-flymake-base--lookup-type-property type
                                                     'mode-line-face
                                                     'compilation-error)
           when (or diags
                    (>= severity (warning-numeric-level :warning)))
           collect `(:propertize
                     ,(format "%d" (length diags))
                     face ,face
                     mouse-face mode-line-highlight
                     keymap
                     ,(let ((map (make-sparse-keymap))
                            (type type))
                        (define-key map (vector 'mode-line
                                                mouse-wheel-down-event)
                          (lambda (event)
                            (interactive "e")
                            (with-selected-window (posn-window (event-start event))
                              (sly-asdf-flymake-base-goto-prev-error 1 (list type) t))))
                        (define-key map (vector 'mode-line
                                                mouse-wheel-up-event)
                          (lambda (event)
                            (interactive "e")
                            (with-selected-window (posn-window (event-start event))
                              (sly-asdf-flymake-base-goto-next-error 1 (list type) t))))
                        map)
                     help-echo
                     ,(concat (format "%s diagnostics of type %s\n"
                                      (propertize (format "%d"
                                                          (length diags))
                                                  'face face)
                                      (propertize (format "%s" type)
                                                  'face face))
                              (format "%s/%s: previous/next of this type"
                                      mouse-wheel-down-event
                                      mouse-wheel-up-event)))
           into forms
           finally return
           `((:propertize "[")
             ,@(cl-loop for (a . rest) on forms by #'cdr
                        collect a when rest collect
                        '(:propertize " "))
             (:propertize "]")))))))

;;; Diagnostics buffer

(defvar-local sly-asdf-flymake-base--diagnostics-buffer-source nil)

(defvar sly-asdf-flymake-base-diagnostics-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'sly-asdf-flymake-base-goto-diagnostic)
    (define-key map (kbd "SPC") 'sly-asdf-flymake-base-show-diagnostic)
    map))

(defun sly-asdf-flymake-base-show-diagnostic (pos &optional other-window)
  "Show location of diagnostic at POS."
  (interactive (list (point) t))
  (let* ((id (or (tabulated-list-get-id pos)
                 (user-error "Nothing at point")))
         (diag (plist-get id :diagnostic)))
    (with-current-buffer (sly-asdf-flymake-base--diag-buffer diag)
      (with-selected-window
          (display-buffer (current-buffer) other-window)
        (goto-char (sly-asdf-flymake-base--diag-beg diag))
        (pulse-momentary-highlight-region (sly-asdf-flymake-base--diag-beg diag)
                                          (sly-asdf-flymake-base--diag-end diag)
                                          'highlight))
      (current-buffer))))

(defun sly-asdf-flymake-base-goto-diagnostic (pos)
  "Show location of diagnostic at POS.
POS can be a buffer position or a button"
  (interactive "d")
  (pop-to-buffer
   (sly-asdf-flymake-base-show-diagnostic (if (button-type pos) (button-start pos) pos))))

(defun sly-asdf-flymake-base--diagnostics-buffer-entries ()
  (with-current-buffer sly-asdf-flymake-base--diagnostics-buffer-source
    (cl-loop for diag in
             (cl-sort (sly-asdf-flymake-base-diagnostics) #'< :key #'sly-asdf-flymake-base-diagnostic-beg)
             for (line . col) =
             (save-excursion
               (goto-char (sly-asdf-flymake-base--diag-beg diag))
               (cons (line-number-at-pos)
                     (- (point)
                        (line-beginning-position))))
             for type = (sly-asdf-flymake-base--diag-type diag)
             collect
             (list (list :diagnostic diag
                         :line line
                         :severity (sly-asdf-flymake-base--lookup-type-property
                                    type
                                    'severity (warning-numeric-level :error)))
                   `[,(format "%s" line)
                     ,(format "%s" col)
                     ,(propertize (format "%s" type)
                                  'face (sly-asdf-flymake-base--lookup-type-property
                                         type 'mode-line-face 'sly-asdf-flymake-base-error))
                     (,(format "%s" (sly-asdf-flymake-base--diag-text diag))
                      mouse-face highlight
                      help-echo "mouse-2: visit this diagnostic"
                      face nil
                      action sly-asdf-flymake-base-goto-diagnostic
                      mouse-action sly-asdf-flymake-base-goto-diagnostic)]))))

(define-derived-mode sly-asdf-flymake-base-diagnostics-buffer-mode tabulated-list-mode
  "Sly-Asdf-Flymake-Base diagnostics"
  "A mode for listing Sly-Asdf-Flymake-Base diagnostics."
  (setq tabulated-list-format
        `[("Line" 5 (lambda (l1 l2)
                      (< (plist-get (car l1) :line)
                         (plist-get (car l2) :line)))
           :right-align t)
          ("Col" 3 nil :right-align t)
          ("Type" 8 (lambda (l1 l2)
                      (< (plist-get (car l1) :severity)
                         (plist-get (car l2) :severity))))
          ("Message" 0 t)])
  (setq tabulated-list-entries
        'sly-asdf-flymake-base--diagnostics-buffer-entries)
  (tabulated-list-init-header))

(defun sly-asdf-flymake-base--diagnostics-buffer-name ()
  (format "*Sly-Asdf-Flymake-Base diagnostics for %s*" (current-buffer)))

(defun sly-asdf-flymake-base-show-diagnostics-buffer ()
  "Show a list of Sly-Asdf-Flymake-Base diagnostics for current buffer."
  (interactive)
  (let* ((name (sly-asdf-flymake-base--diagnostics-buffer-name))
         (source (current-buffer))
         (target (or (get-buffer name)
                     (with-current-buffer (get-buffer-create name)
                       (sly-asdf-flymake-base-diagnostics-buffer-mode)
                       (setq sly-asdf-flymake-base--diagnostics-buffer-source source)
                       (current-buffer)))))
    (with-current-buffer target
      (revert-buffer)
      (display-buffer (current-buffer)))))

(provide 'sly-asdf-flymake-base)

;;(require 'flymake-proc)

;;; sly-asdf-flymake-base.el ends here
