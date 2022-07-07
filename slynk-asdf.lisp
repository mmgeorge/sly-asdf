;;; Slynk-asdf.lisp -- ASDF support
;;; Ported from swank-asdf <https://github.com/slime/slime/blob/master/contrib/swank-asdf.lisp>
;; Authors: Daniel Barlow <dan@telent.net>
;;          Marco Baringer <mb@bese.it>
;;          Edi Weitz <edi@agharta.de>
;;          Francois-Rene Rideau <tunes@google.com>
;;          and others
;; License: Public Domain


(defpackage :slynk-asdf
  (:use :cl :slynk-api :slynk-backend)
  (:import-from :slynk)
  (:export #:make-output-translation-function
           #:while-collecting-notes
           #:*current-system-buffers*))

(in-package :slynk-asdf)


(defvar *recompile-system* nil)
(defvar *pathname-component* (make-hash-table :test 'equal))
(defvar *current-source-file* nil)
(defvar *asdf-condition-types*
  '(;; System definition related
    #-ecl asdf/parse-defsystem:bad-system-name
    asdf:load-system-definition-error
    ;; asdf/plan::dependency-not-done
    uiop/lisp-build:compile-file-error
    uiop/lisp-build:compile-warned-warning))
(defvar *current-system-buffers*
  :documentation "List of current open sly buffers. We call load-source-op 
on these directly to improve load-time error messages")


;;; Macros

(defmacro while-collecting ((&rest collectors) &body body)
  `(asdf::while-collecting ,collectors ,@body))

(defmacro while-collecting-notes ((&key interactive) &body body)
  `(collect-notes (lambda () ,@body) ,interactive))

(defmacro asdefs (version &rest defs)
  (flet ((defun* (version name aname rest)
           `(progn
              (defun ,name ,@rest)
              (declaim (notinline ,name))
              (when (asdf-at-least ,version)
                (setf (fdefinition ',name) (fdefinition ',aname)))))
         (defmethod* (version aname rest)
           `(unless (asdf-at-least ,version)
              (defmethod ,aname ,@rest)))
         (defvar* (name aname rest)
           `(progn
              (define-symbol-macro ,name ,aname)
              (defvar ,aname ,@rest))))
    `(progn
       ,@(loop :for (def name . args) :in defs
            :for aname = (intern (string name) :asdf)
            :collect
              (ecase def
                ((defun) (defun* version name aname args))
                ((defmethod) (defmethod* version aname args))
                ((defvar) (defvar* name aname args)))))))


;;; Callable by RPC

(defslyfun who-depends-on (system)
  (flet ((system-dependencies (op system)
           (mapcar (lambda (dep)
                     (asdf::coerce-name (if (consp dep) (second dep) dep)))
                   (cdr (assoc op (asdf:component-depends-on op system))))))
    (let ((system-name (asdf::coerce-name system))
          (result))
      (asdf::map-systems
       (lambda (system)
         (when (member system-name
                       (system-dependencies 'asdf:load-op system)
                       :test #'string=)
           (push (asdf:component-name system) result))))
      result)))


(defslyfun operate-on-system-for-emacs (system-name operation &rest keywords)
  "Compile and load SYSTEM using ASDF.
Record compiler notes signalled as `compiler-condition's."
  (slynk-asdf:while-collecting-notes (:interactive nil)
                                     ;;(slynk::collect-notes
                                     (apply #'operate-on-system system-name operation keywords)))


(defslyfun list-all-systems-in-central-registry ()
  "Returns a list of all systems in ASDF's central registry
AND in its source-registry. (legacy name)"
  (unique-string-list
   (mapcar
    #'pathname-name
    (while-collecting (c)
                      (loop for dir in asdf:*central-registry*
                         for defaults = (eval dir)
                         when defaults
                         do (collect-asds-in-directory defaults #'c))
                      (asdf:ensure-source-registry)
                      (if (or #+asdf3 t
                              #-asdf3 (asdf:version-satisfies (asdf:asdf-version) "2.15"))
                          (loop :for k :being :the :hash-keys :of asdf::*source-registry*
                             :do (c k))
                          #-asdf3
                          (dolist (entry (asdf::flatten-source-registry))
                            (destructuring-bind (directory &key recurse exclude) entry
                              (register-asd-directory
                               directory
                               :recurse recurse :exclude exclude :collect #'c))))))))


(defslyfun list-all-systems-known-to-asdf ()
  "Returns a list of all systems ASDF knows already."
  (while-collecting (c)
                    (asdf::map-systems (lambda (system) (c (asdf:component-name system))))))


(defslyfun list-asdf-systems ()
  "Returns the systems in ASDF's central registry and those which ASDF
already knows."
  (unique-string-list
   (list-all-systems-known-to-asdf)
   (list-all-systems-in-central-registry)))


(defslyfun asdf-system-files (name)
  (let* ((system (asdf:find-system name))
         (files (mapcar #'namestring
                        (cons
                         (asdf:system-source-file system)
                         (asdf-component-source-files system))))
         (main-file (find name files
                          :test #'equalp :key #'pathname-name :start 1)))
    (if main-file
        (cons main-file (remove main-file files
                                :test #'equal :count 1))
        files)))


(defslyfun reload-system (name)
  (let ((*recompile-system* (asdf:find-system name)))
    (operate-on-system-for-emacs name 'asdf:load-op)))


(defslyfun asdf-system-loaded-p (name)
  (component-loaded-p name))


(defslyfun asdf-system-directory (name)
  (namestring (translate-logical-pathname (asdf:system-source-directory name))))


(defslyfun asdf-determine-system (file buffer-package-name)
  (or
   (and file
        (pathname-system file))
   (and file
        (progn
          ;; If not found, let's rebuild the table first
          (recompute-pathname-component-table)
          (pathname-system file)))
   ;; If we couldn't find an already defined system,
   ;; try finding a system that's named like BUFFER-PACKAGE-NAME.
   (loop with package = (guess-buffer-package buffer-package-name)
      for name in (slynk::package-names package)
      for system = (asdf:find-system (asdf::coerce-name name) nil)
      when (and system
                (or (not file)
                    (pathname-system file)))
      return (asdf:component-name system))))


(defslyfun delete-system-fasls (name)
  (let ((removed-count
         (loop for file in (asdf-component-output-files
                            (asdf:find-system name))
            when (probe-file file)
            count it
            and
            do (delete-file file))))
    (format nil "~d file~:p ~:*~[were~;was~:;were~] removed" removed-count)))

(defun collect-notes (function interactive)
  "Includes additional asdf specific logic from `slynk::collect-notes`"
  (let ((notes '())
        (restart-p nil)) ;; Denotes a critical failure
    ;; Filter unbound-variable errors that have a corresponding undefined-variable warning.
    ;; The undefined-variable warning provides a better warning with a precise source error
    ;; location (at least on SBCL)
    (flet ((redundant-note-p (note)
             (when (eq (getf note :type) 'unbound-variable)
               (let* ((message (getf note :message))
                      (start 13) ;; "The variable "
                      (end (search " is unbound" message))
                      (var-name (subseq message start end)))
                 (find-if (lambda (prev-note)
                            (let ((type (getf prev-note :type))
                                  (message (getf prev-note :message)))
                              (and (eq type 'simple-warning)
                                   (search var-name message))))
                          notes))))
           (error-p (c) (typep (slynk-backend:original-condition c) 'error)))
      (multiple-value-bind (result seconds)
          (handler-bind ((slynk::compiler-condition
                          (lambda (c)
                            (let ((note (make-compiler-note c)))
                              (when note
                                (push note notes)))
                            (when (and (error-p c) (not interactive)) ;; For warnings, we do not need to restart
                              ;; We may be loading a system in which case we would
                              ;; have an accept restart that we can use to continue
                              ;; compilation. Either way, mark has having restarted
                              (setf restart-p t)
                              ;; Recovering from errors can be fairly unintuitive
                              ;; see :cascading-errors system and issue #29. Disable for now
                              ;; (let ((accept (find-restart 'asdf/action:accept)))
                              ;;   (if accept
                              ;;       (invoke-restart accept)
                              ;;       (invoke-restart 'abort)))
                              (invoke-restart 'abort)))))
            (slynk::measure-time-interval
             (lambda ()
               ;; To report location of error-signaling toplevel forms
               ;; for errors in EVAL-WHEN or during macroexpansion.
               (restart-case (multiple-value-list (funcall function))
                 (abort () :report "Abort compilation." (list nil))))))
        (let ((notes (remove-if #'redundant-note-p notes)))
          (slynk::make-compilation-result :notes notes
                                          :duration seconds
                                          :successp (not restart-p)
                                          :loadp t
                                          :faslfile nil))))))
        

(defun asdf-condition-p (condition)
  (member (type-of condition) *asdf-condition-types*))


(defun make-compiler-note (condition)
  "Make a compiler note data structure from a compiler-condition."
  (if (asdf-condition-p (original-condition condition))
      (make-asdf-note (original-condition condition))
      (unless (member (severity condition) '(:redefinition))
        (list* :message (message condition)
               :severity (severity condition)
               ;; MG: If we were unable to get the precise source location, replace with
               ;; *current-source-file* which we dynamically bound around asdf:load*
               :location (replace-location-if-error (location condition)) 
               :references (references condition)
               :type (type-of (original-condition condition))
               :asdf nil
               (let ((s (source-context condition)))
                 (if s (list :source-context s)))))))


(defun replace-location-if-error (location)
  (if (and (eq (car location) :error)
           *current-source-file*)
      (slynk-backend:make-location
       `(:file ,(namestring (translate-logical-pathname (asdf:component-pathname *current-source-file*))))
       `(:position 0))
      location))


(defun make-asdf-note (condition)
  (etypecase condition
    ((or asdf:load-system-definition-error
         asdf:system-definition-error)
     (let ((message (format nil "~A" (asdf/find-system:error-condition condition))))
       ;; Clobber asdf messages in quicklisp deps. Better would be to extract the
       ;; file buffer and delegate to the calling code to clobber irrelevant errors
       (unless (search "/quicklisp" message)
         (list* :message message
                :severity :error
                :location nil
                :references nil
                :type (type-of condition)
                :asdf t))))
    ;; Clobber for now
    ((or #-ecl asdf/parse-defsystem:bad-system-name
         ;; MG: asdf/plan::dependency-not-done requires 3.3.1.4.
         ;; Be sure to also uncomment the corresponding statement in *asdf-condition-types*
         ;; https://github.com/fare/asdf/commit/6cba911f89e15bcde00cced5248190b4d747ab90
         ;; asdf/plan::dependency-not-done
         uiop/lisp-build:compile-warned-warning) ;; "Lisp compilation had style-warnings while compiling ..."
     nil)
    (uiop/lisp-build:compile-file-error nil)))



(defun parse-condition-location (condition)
  "Inspect the class of a given CONDITION. If it includes a slot with the name LOCATION
return it if it is of the form (:file FILENAME :pos NUMBER)"
  (loop for slot in (slynk-mop:class-direct-slots (class-of condition))
     for name = (slynk-mop:slot-definition-name slot)
     when (string-equal name "LOCATION") :do
       (return-from parse-condition-location
         (slynk-mop:slot-value-using-class  (class-of condition) condition slot))))


(defslyfun compile-files-for-flymake (filenames)
  (let (;; Muffle compilation
        (*standard-output* (make-string-output-stream))
        (*error-output* (make-string-output-stream)))
    `(:compilation-result 
      ,(mapcan #'(lambda (filename) (cadr (slynk:compile-file-for-emacs filename nil))) filenames))))



(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (let ((*current-source-file* c))
    (call-next-method)))


;; (defmethod asdf/plan:record-dependency :around (plan operation component)
;;   ;; load-op provides significantly worse debugging information as it involves
;;   ;; loading fasls rather than the source code directly. On SBCL, loading a fasl
;;   ;; will fail to capture the context for top-level errors.
;;   ;;
;;   ;; e.g
;;   ;; (setf undefined-var 2) ;; Throws an error on load, but source context is NIL
;;   ;; 
;;   ;; This prevents us
;;   ;; from being able to given a line number for the error. To get around this,
;;   ;; we keep track of a list of *CURRENT-SYSTEM-BUFFERS*
;;   (let ((*current-component* component))
;;     (let ((op (if (typep operation 'asdf:load-op)
;;                   (asdf:make-operation 'asdf:load-source-op)
;;                   operation)))
;;       (call-next-method plan op component))))


(defun make-relative-pathname (pathname)
  (make-pathname
   :name (pathname-name pathname)
   :type (pathname-type pathname)
   :directory (cons :relative (cdr (pathname-directory pathname)))))


(defun flymake-fasl-directory ()
  (let ((pathname (merge-pathnames  ".slynk/asdf/fasl/" (user-homedir-pathname))))
    (ensure-directories-exist pathname)
    pathname))


(defun make-output-translation-function ()
  (lambda (x)
    (let ((pathname (merge-pathnames 
                     (make-relative-pathname x)
                     (flymake-fasl-directory))))
      ;;(format t "Compile it! ~A~%" pathname)
      pathname)))


(defslyfun remove-flymake-fasls ()
  ;;(uiop:delete-directory-tree (flymake-fasl-directory) :validate t)
  nil)

(defslyfun compile-system-for-flymake (name buffers)
  ;; Bad things happen because we wind up loading slynk twice
  (when (search "slynk" name)
    (return-from compile-system-for-flymake))
  ;; Muffle compilation
  ;;(uiop:delete-directory-tree (flymake-fasl-directory) :validate t)
  (let ((*standard-output* (make-string-output-stream))
        (*error-output* (make-string-output-stream))
        (asdf/driver:*output-translation-function*
         (lambda (x)
           (let ((pathname (merge-pathnames 
                            (make-relative-pathname x)
                            (flymake-fasl-directory))))
             pathname))))
    
    (check-compilation-errors-for-system name buffers)))


(defun check-compilation-errors-for-system (system buffers)
  (let ((stream (make-string-output-stream)))
    (spawn-compilation-process-for-system system buffers :stream stream)
    (parse-compilation-result (get-output-stream-string stream))))


(defun parse-compilation-result (string)
  (handler-bind ((error
                  (lambda (e)
                    (declare (ignore e))
                    ;; We may attempt to intern error types of packages that
                    ;; do not exist in the current context (e.g.: ALEXANDRIA.DEV.0::SIMPLE-STYLE-WARNING)
                    ;; This will capture that error and instead use the current package
                    (invoke-restart (fourth (compute-restarts)))
                    )))
    (read-from-string (subseq string (search "(:COMPILATION-RESULT" string)))))


(defun spawn-compilation-process-for-system (system buffers &key stream)
  (uiop/driver:run-program
   `("sbcl" "--non-interactive"
            ;; Transfer the current value for the *central-repository*. This includes the current
            ;; location of `slynk` files allowing us to asdf:load-system slynk in our compilation process
            "--eval" ,(with-output-to-string (s)
                        (print-object `(setf asdf:*central-registry* ',asdf:*central-registry*) s))
            "--eval" "(asdf:load-system \"slynk\")"
            "--eval" "(asdf:load-system \"slynk-asdf\")"
            "--eval" ,(with-output-to-string (s)
                        (print-object `(setf slynk-asdf:*current-system-buffers* ',buffers) s))
            "--eval" "(in-package :slynk-asdf)"
            "--eval" ,(with-output-to-string (s)
                        (print-object
                         ;; Send output to *standard-output* for parsing later
                         `(progn
                            (format *standard-output* "~%") ;; Ensure new line
                            (print-object
                             (let ((asdf/driver:*output-translation-function*
                                    (slynk-asdf:make-output-translation-function))
                                   (*standard-output* (make-string-output-stream)))
                               (slynk-asdf::operate-on-system-for-emacs ,system 'load-op :force t))
                             *standard-output*))
                         s)))
   :error-output *standard-output*
   :output stream))





;;; Internal

(defun asdf-at-least (version)
  (asdf:version-satisfies (asdf:asdf-version) version))


(asdefs
 "2.15"
 (defvar *wild* #-cormanlisp :wild #+cormanlisp "*")

 (defun collect-asds-in-directory (directory collect)
   (map () collect (directory-asd-files directory)))
 
 (defun register-asd-directory (directory &key recurse exclude collect)
   (if (not recurse)
       (collect-asds-in-directory directory collect)
       (collect-sub*directories-asd-files
        directory :exclude exclude :collect collect))))


(asdefs
 "2.16"
 (defun directory* (pathname-spec &rest keys &key &allow-other-keys)
   (apply 'directory pathname-spec
          (append keys
                  '#.(or #+allegro
                         '(:directories-are-files nil
                           :follow-symbolic-links nil)
                         #+clozure
                         '(:follow-links nil)
                         #+clisp
                         '(:circle t :if-does-not-exist :ignore)
                         #+(or cmu scl)
                         '(:follow-links nil :truenamep nil)
                         #+sbcl
                         (when (find-symbol "RESOLVE-SYMLINKS" '#:sb-impl)
                           '(:resolve-symlinks nil)))))))


(asdefs
 "2.17"
 (defun collect-sub*directories-asd-files
     (directory &key
                  (exclude asdf::*default-source-registry-exclusions*)
                  collect)
   (asdf::collect-sub*directories
    directory
    (constantly t)
    (lambda (x) (not (member (car (last (pathname-directory x)))
                             exclude :test #'equal)))
    (lambda (dir) (collect-asds-in-directory dir collect))))

 (defun system-source-directory (system-designator)
   (asdf::pathname-directory-pathname
    (asdf::system-source-file system-designator)))

 (defun filter-logical-directory-results (directory entries merger)
   (if (typep directory 'logical-pathname)
       (loop for f in entries
          when
            (if (typep f 'logical-pathname)
                f
                (let ((u (ignore-errors (funcall merger f))))
                  (and u
                       (equal (ignore-errors (truename u))
                              (truename f))
                       u)))
          collect it)
       entries))

 (defun directory-asd-files (directory)
   (directory-files directory asdf::*wild-asd*)))


(asdefs
 "2.19"
 (defun subdirectories (directory)
   (let* ((directory (asdf::ensure-directory-pathname directory))
          #-(or abcl cormanlisp xcl)
          (wild (asdf::merge-pathnames*
                 #-(or abcl allegro cmu lispworks sbcl scl xcl)
                 asdf::*wild-directory*
                 #+(or abcl allegro cmu lispworks sbcl scl xcl) "*.*"
                 directory))
          (dirs
           #-(or abcl cormanlisp xcl)
           (ignore-errors
             (directory* wild . #.(or #+clozure '(:directories t :files nil)
                                      #+mcl '(:directories t))))
           #+(or abcl xcl) (system:list-directory directory)
           #+cormanlisp (cl::directory-subdirs directory))
          #+(or abcl allegro cmu lispworks sbcl scl xcl)
          (dirs (loop for x in dirs
                   for d = #+(or abcl xcl) (extensions:probe-directory x)
                     #+allegro (excl:probe-directory x)
                     #+(or cmu sbcl scl) (asdf::directory-pathname-p x)
                     #+lispworks (lw:file-directory-p x)
                   when d collect #+(or abcl allegro xcl) d
                     #+(or cmu lispworks sbcl scl) x)))
     (filter-logical-directory-results
      directory dirs
      (let ((prefix (or (normalize-pathname-directory-component
                         (pathname-directory directory))
                        ;; because allegro 8.x returns NIL for #p"FOO:"
                        '(:absolute))))
        (lambda (d)
          (let ((dir (normalize-pathname-directory-component
                      (pathname-directory d))))
            (and (consp dir) (consp (cdr dir))
                 (make-pathname
                  :defaults directory :name nil :type nil :version nil
                  :directory
                  (append prefix
                          (make-pathname-component-logical
                           (last dir))))))))))))


(asdefs
 "2.21"
 (defun component-loaded-p (c)
   (and (gethash 'load-op (asdf::component-operation-times
                           (asdf::find-component c nil))) t))

 (defun normalize-pathname-directory-component (directory)
   (cond
     #-(or cmu sbcl scl)
     ((stringp directory) `(:absolute ,directory) directory)
     ((or (null directory)
          (and (consp directory)
               (member (first directory) '(:absolute :relative))))
      directory)
     (t
      (error "Unrecognized pathname directory component ~S" directory))))

 (defun make-pathname-component-logical (x)
   (typecase x
     ((eql :unspecific) nil)
     #+clisp (string (string-upcase x))
     #+clisp (cons (mapcar 'make-pathname-component-logical x))
     (t x)))

 (defun make-pathname-logical (pathname host)
   (make-pathname
    :host host
    :directory (make-pathname-component-logical (pathname-directory pathname))
    :name (make-pathname-component-logical (pathname-name pathname))
    :type (make-pathname-component-logical (pathname-type pathname))
    :version (make-pathname-component-logical (pathname-version pathname)))))


(asdefs
 "2.22"
 (defun directory-files (directory &optional (pattern asdf::*wild-file*))
   (let ((dir (pathname directory)))
     (when (typep dir 'logical-pathname)
       (when (wild-pathname-p dir)
         (error "Invalid wild pattern in logical directory ~S" directory))
       (unless (member (pathname-directory pattern)
                       '(() (:relative)) :test 'equal)
         (error "Invalid file pattern ~S for logical directory ~S"
                pattern directory))
       (setf pattern (make-pathname-logical pattern (pathname-host dir))))
     (let ((entries (ignore-errors
                      (directory* (asdf::merge-pathnames* pattern dir)))))
       (filter-logical-directory-results
        directory entries
        (lambda (f)
          (make-pathname :defaults dir
                         :name (make-pathname-component-logical
                                (pathname-name f))
                         :type (make-pathname-component-logical
                                (pathname-type f))
                         :version (make-pathname-component-logical
                                   (pathname-version f)))))))))


(asdefs
 "2.26.149"
 (defmethod component-relative-pathname ((system asdf:system))
   (asdf::coerce-pathname
    (and (slot-boundp system 'asdf::relative-pathname)
         (slot-value system 'asdf::relative-pathname))
    :type :directory
    :defaults (system-source-directory system)))
 (defun load-asd (pathname &key name &allow-other-keys)
   (asdf::load-sysdef (or name (string-downcase (pathname-name pathname)))
                      pathname)))



(defun asdf-operation (operation)
  (or (asdf::find-symbol* operation :asdf)
      (error "Couldn't find ASDF operation ~S" operation)))

(defun map-system-components (fn system)
  (map-component-subcomponents fn (asdf:find-system system)))

(defun map-component-subcomponents (fn component)
  (when component
    (funcall fn component)
    (when (typep component 'asdf:module)
      (dolist (c (asdf:module-components component))
        (map-component-subcomponents fn c)))))


;;; Maintaining a pathname to component table

(defun clear-pathname-component-table ()
  (clrhash *pathname-component*))

(defun register-system-pathnames (system)
  (map-system-components 'register-component-pathname system))

(defun recompute-pathname-component-table ()
  (clear-pathname-component-table)
  (asdf::map-systems 'register-system-pathnames))

(defun pathname-component (x)
  (gethash (pathname x) *pathname-component*))

(defmethod asdf:component-pathname :around ((component asdf:component))
  (let ((p (call-next-method)))
    (when (pathnamep p)
      (setf (gethash p *pathname-component*) component))
    p))

(defun register-component-pathname (component)
  (asdf:component-pathname component))

(recompute-pathname-component-table)


;;; This is a crude hack, see ASDF's LP #481187.

(defmethod xref-doit ((type (eql :depends-on)) thing)
  (when (typep thing '(or string symbol))
    (loop for dependency in (who-depends-on thing)
       for asd-file = (asdf:system-source-file dependency)
       when asd-file
       collect (list dependency
                     (slynk-backend:make-location
                      `(:file ,(namestring asd-file))
                      `(:position 1)
                      `(:snippet ,(format nil "(defsystem :~A" dependency)
                                 :align t))))))

(defun foo (a)
  (bar a))

(defun foo2 (a)
  (step
   (bar a)))

(defun bar (a)
  (break)
  (format t "~A" (+ 1 a)))


#+sbcl
(defun breakpoint-add (function)
  "Warning! SBCL only"
  ;; Or should we be recompiling function with a (break) in the proper location?
  (labels ((call-with-breakpoint (func &rest args) (step (apply func args))))
    (unless (sb-int:encapsulated-p function 'breakpoint)
      (sb-int:encapsulate function 'breakpoint #'call-with-breakpoint))))

#+sbcl
(defun breakpoint-remove (function)
  (when (sb-int:encapsulated-p function 'breakpoint)
    (sb-int:unencapsulate function 'breakpoint)))


(defun operate-on-system (system-name operation-name &rest keyword-args)
  "Perform OPERATION-NAME on SYSTEM-NAME using ASDF.
The KEYWORD-ARGS are passed on to the operation.
Example:
\(operate-on-system \"cl-ppcre\" 'compile-op :force t)"
  ;;(handler-case
  (slynk-backend:with-compilation-hooks ()
    (apply #'asdf:operate (asdf-operation operation-name)
           system-name keyword-args)))
;;((or asdf:compile-error #+asdf3 asdf/lisp-build:compile-file-error)
;; () nil)))


(defun make-operation (x)
  #+#.(slynk-backend:with-symbol 'make-operation 'asdf)
  (asdf:make-operation x)
  #-#.(slynk-backend:with-symbol 'make-operation 'asdf)
  (make-instance x))


(defmethod asdf:operation-done-p :around
    ((operation asdf:compile-op)
     component)
  (unless (eql *recompile-system*
               (asdf:component-system component))
    (call-next-method)))


(defun unique-string-list (&rest lists)
  (sort (delete-duplicates (apply #'append lists) :test #'string=) #'string<))


(defun asdf-component-source-files (component)
  (if (and #+asdf3 (typep component 'asdf:package-inferred-system))
      (asdf-inferred-system-files component ".lisp")
      (while-collecting (c)
                        (labels ((f (x)
                                   (typecase x
                                     (asdf:source-file (c (asdf:component-pathname x)))
                                     (asdf:module (map () #'f (asdf:module-components x))))))
                          (f component)))))


(defun asdf-component-output-files (component)
  (if (and #+asdf3 (typep component 'asdf:package-inferred-system))
      (asdf-inferred-system-files component ".fasl")
      (while-collecting (c)
                        (labels ((f (x)
                                   (typecase x
                                     (asdf:source-file
                                      (map () #'c
                                           (asdf:output-files (make-operation 'asdf:compile-op) x)))
                                     (asdf:module (map () #'f (asdf:module-components x))))))
                          (f component)))))


(defun asdf-inferred-system-files (system ending)
  (let ((system-pathname (asdf:component-pathname system)))
    (flet ((dep-pathname (dep)
             (let ((name (asdf:component-name dep)))
               (parse-namestring (concatenate
                                  'string
                                  (namestring system-pathname )
                                  (subseq name (1+ (position #\/ name)))
                                  ending)))))
      (mapcar #'dep-pathname (asdf-inferred-system-deps system)))))


(defun asdf-inferred-system-deps (system &optional (visited nil))
  ;; A "proper-dep" is a dependency with the same primary system name (i.e., not an external library)
  (flet ((proper-dep-p (dep) (string-equal (asdf:primary-system-name dep) (asdf:primary-system-name system)))
         (recurse (prev dep) (asdf-inferred-system-deps dep prev)))
    (let* ((proper-deps (remove-if-not #'proper-dep-p (mapcar #'asdf:find-system (asdf:system-depends-on system))))
           (unvisited-deps (set-difference proper-deps visited :test #'component-equal)))
      (reduce #'recurse unvisited-deps :initial-value (append unvisited-deps visited)))))


(defun component-equal (component other)
  (string-equal (asdf:component-name component)
                (asdf:component-name other)))


(defun pathname-system (pathname)
  (let ((component (pathname-component pathname)))
    (when component
      (asdf:component-name (asdf:component-system component)))))


;; MG: slynk:*compile-for-emacs-hook* is not exposed
;; (defun try-compile-file-with-asdf (pathname load-p &rest options)
;;   (declare (ignore options))
;;   (let ((component (pathname-component pathname)))
;;     (when component
;;       ;;(format t "~&Compiling ASDF component ~S~%" component)
;;       (let ((op (make-operation 'asdf:compile-op)))
;;         (with-compilation-hooks ()
;;           (asdf:perform op component))
;;         (when load-p
;;           (asdf:perform (make-operation 'asdf:load-op) component))
;;         (values t t nil (first (asdf:output-files op component)))))))


;; (defun try-compile-asd-file (pathname load-p &rest options)
;;   (declare (ignore load-p options))
;;   (when (equalp (pathname-type pathname) "asd")
;;     (load-asd pathname)
;;     (values t t nil pathname)))
