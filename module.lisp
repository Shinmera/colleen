#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defvar *bot-modules* (make-hash-table) "Global module table consisting of name->instance pairs.")
(defvar *current-module*)
(setf (documentation '*current-module* 'variable) "Special variable containing the module in the current module context.")

(defclass module ()
  ((%active :initform NIL :accessor active :allocation :class)
   (%threads :initform (make-hash-table :test 'equalp) :accessor threads :allocation :class)
   (%lock :initform (bordeaux-threads:make-lock) :accessor lock :allocation :class)
   (%storage :initform (make-hash-table :test 'equal) :accessor storage :allocation :class))
  (:documentation "Base module class."))

(defmacro generalize-module-accessor (name)
  `(progn
     (defmethod ,name ((module-name string))
       (,name (get-module module-name)))
     (defmethod ,name ((module-name symbol))
       (,name (get-module module-name)))))

(generalize-module-accessor active)
(generalize-module-accessor threads)

(defmethod print-object ((module module) stream)
  (format stream "<[~a]>" (class-name (class-of module))))

(defgeneric start (module)
  (:documentation "Start the module and activate it for use.")
  (:method ((module module)))
  (:method :around ((module module))
    (load-storage module)
    (call-next-method)
    (setf (active module) T)
    module))

(defgeneric stop (module)
  (:documentation "Stop the module and attempt to clean everything up.")
  (:method ((module module)))
  (:method :around ((module module))
    (setf (active module) NIL)
    (loop for uid being the hash-keys of (threads module)
          for thread being the hash-values of (threads module)
          do (if (thread-alive-p thread)
                 (interrupt-thread thread #'(lambda () (error 'module-stop)))
                 (remhash uid (threads module))))
    (call-next-method)
    (save-storage module)
    module))

(defun module-thread (module uuid)
  "Returns the thread identified by UUID on MODULE or NIL if none is found."
  (gethash uuid (threads (get-module module))))

(defgeneric (setf module-thread) (thread module uuid)
  (:documentation "Sets the UUID on the module to the specified thread.
If a thread already exists at the specified UUID, a warning is logged.")
  (:method (thread (module module) (uuid string))
    (let ((threads (threads (get-module module))))
      (when (gethash uuid threads)
        (v:warn :module "Replacing ~a's already existing and potentially running thread ~a!" module uuid))
      (setf (gethash uuid threads)
            thread))))

(defun stop-module-thread (module uuid)
  "Stops the thread identified by UUID from the MODULE.
The thread will most likely remove itself once it stops.
It is not guaranteed that the thread will stop immediately."
  (let ((thread (module-thread module uuid)))
    (when (and thread (thread-alive-p thread))
      (interrupt-thread thread #'(lambda () (error 'module-stop))))))

(defun remove-module-thread (module uuid &key keep-alive)
  "Removes the thread identified by UUID from the MODULE.
If KEEP-ALIVE is non-NIL and the thread is alive, it is only removed.
Otherwise if it is still alive, the thread is stopped and removed."
  (let* ((module (get-module module))
         (thread (gethash uuid (threads module))))
    (when thread
      (when (and keep-alive (thread-alive-p thread))
        (v:warn :module "Stopping ~a's thread ~a due to removal." module uuid)
        (interrupt-thread thread #'(lambda () (error 'module-stop))))
      (remhash uuid (threads module)))))

(defmacro with-module ((var &optional (name (get-current-module-name))) &body forms)
  "Executes the forms in a context where VAR is bound to the module instance named by NAME.
This also binds *CURRENT-MODULE*."
  `(let* ((,var (get-module ,name))
          (*current-module* ,var))
     ,@forms))

(defmacro with-module-thread ((&optional (module '*current-module*)) &body thread-body)
  "Executes the THREAD-BODY in a separate thread bound to the MODULE.
The return value of this is the new thread's UUID string.

The thread contains implicit condition handling constructs:
When an error is caught at the lowest level and *DEBUGGER* is non-NIL,
then the error is passed to INVOKE-DEBUGGER. If the condition is not
handled or when the thread body finishes executing, the thread is ended
and it is removed from the module's threads table."
  (let ((uidgens (gensym "UUID"))
        (modgens (gensym "MODULE"))
        (modnamegens (gensym "MODULE-NAME")))
    `(let* ((,uidgens (princ-to-string (uuid:make-v4-uuid)))
            (,modgens (get-module ,module))
            (,modnamegens (name ,modgens)))
       (setf (module-thread ,modgens ,uidgens)
             (make-thread #'(lambda ()
                              (unwind-protect
                                   (handler-case
                                       (handler-bind
                                           ((error #'(lambda (err)
                                                       (v:severe ,modnamegens "Unexpected error at thread-level: ~a" err)
                                                       (when *debugger*
                                                         (invoke-debugger err)))))
                                         ,@thread-body)
                                     (error (err)
                                       (declare (ignore err)))
                                     (module-stop (err)
                                       (declare (ignore err))
                                       (v:debug ,modnamegens "Received module-stop condition, leaving thread ~a." ,uidgens)))
                                (v:trace ,modnamegens "Ending thread ~a." ,uidgens)
                                (remhash ,uidgens (threads ,modgens))))
                          :initial-bindings (loop for symbol in '(*current-server* *current-module* uc:*config*)
                                                  when (boundp symbol)
                                                    collect (cons symbol (symbol-value symbol)))))
       ,uidgens)))

(defmacro with-module-lock ((&optional (module '*current-module*) (lockvar (gensym "LOCK"))) &body forms)
  "Creates a context with the module's lock held.
The FORMS are only executed once the lock has been acquired.
This is an implicit PROGN.

MODULE  --- The module to use the lock of.
LOCKVAR --- A symbol the lock is bound to.
FORMS   ::= form*"
  `(let ((,lockvar (lock (get-module ,module))))
     (bordeaux-threads:with-lock-held (,lockvar)
       ,@forms)))

(defun print-module-thread-stats ()
  "Prints all modules that have recorded threads and whether the threads are active (.) or dead (x)."
  (loop for v being the hash-values of *bot-modules*
        when (< 0 (hash-table-count (threads v)))
          do (format T "~25a ~2a " v (hash-table-count (threads v)))
             (loop for tv being the hash-values of (threads v)
                   do (format T "~:[x~;.~]" (bt:thread-alive-p tv)))
             (format T "~%")))

(defun sweep-module-threads (module)
  "Sweeps the module's threads table and removes all dead threads.
Returns two values: how many threads were removed and how many remain."
  (let* ((module (get-module module))
         (threads (threads module))
         (count 0))
    (loop for k being the hash-keys of threads
          for v being the hash-values of threads
          unless (thread-alive-p v)
            do (remhash k threads)
               (incf count))
    (let ((remaining (hash-table-count threads)))
      (v:debug (name module) "Sweeped threads. ~d removed, ~d still active." count remaining)
      (values count remaining))))

(defun sweep-all-module-threads ()
  "Performs SWEEP-MODULE-THREADS on all modules."
  (loop for v being the hash-values of *bot-modules*
        do (sweep-module-threads v)))

(defgeneric to-module-name (name-object)
  (:documentation "Attempts to transform the given object into the keyword name for a module.")
  (:method ((module-instance module))
    (to-module-name (princ-to-string (class-name (class-of module-instance)))))
  
  (:method ((module-name string))
    (find-symbol (string-upcase module-name) :KEYWORD))

  (:method ((module-name symbol))
    (if (keywordp module-name)
        module-name
        (to-module-name (symbol-name module-name)))))

(defun get-module (designator)
  "Returns the current class instance of the module."
  (gethash (to-module-name designator) *bot-modules*))

(defmethod name ((module module))
  (find-symbol (princ-to-string (class-name (class-of module))) :KEYWORD))

(defun package-symbol (package)
  "Returns the symbol of a package."
  (let ((name (package-name package)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))

(defun get-current-module (&optional (package *package*))
  "Returns the module of the current package context."
  (get-module (get (package-symbol package) :module)))

(defun get-current-module-name (&optional (package *package*))
  "Returns the name of the module in the current package context."
  (get (package-symbol package) :module))

(defmacro define-module (name direct-superclasses direct-slots &body options)
  "Define a new module class. See DEFCLASS.
Note that all module slots are always allocated on the class."
  (let ((keyname (intern (string-upcase name) "KEYWORD")))
    `(eval-when (:compile-toplevel :load-toplevel :execute) 
       (defclass ,name (module ,@direct-superclasses)
         ((%active :initform NIL :reader active :allocation :class)
          (%threads :initform (make-hash-table :test 'equalp) :accessor threads :allocation :class)
          (%lock :initform (bordeaux-threads:make-lock ,(string name)) :accessor lock :allocation :class)
          (%storage :initform (make-hash-table :test 'equal) :accessor storage :allocation :class)
          ,@(mapcar #'(lambda (slot) (append slot '(:allocation :class))) direct-slots))
         ,@options)
       (when (and (gethash ,keyname *bot-modules*)
                  (active (gethash ,keyname *bot-modules*)))
         (v:warn :colleen "Redefining started module ~a. Let's hope everything goes well..." ,keyname))
       (setf (get (package-symbol *package*) :module) ,keyname
             (gethash ,keyname *bot-modules*) (make-instance ',name)))))

(defun start-module (&rest module-names)
  "Start up one or more modules. Each module name should be a symbol or string.

The following restarts are available:
  SKIP  --- Skip starting the module.
  FORCE --- Force starting even though it's already active.
  RETRY --- Simply retry starting."
  (dolist (module-name module-names)
    (setf module-name (to-module-name module-name))
    (with-simple-restart (skip "Skip starting the module.")
      (let ((module (get-module module-name)))
        (assert (not (null module)) () "Module ~a not found!" module-name)
        (with-simple-restart (force "Force starting the module.")
          (assert (not (active module)) () "Module ~a already started!" module-name))
        (v:info module-name "Starting...")
        (loop until (with-simple-restart (retry "Retry starting the module.")
                      (start module)))
        module))))

(defun stop-module (&rest module-names)
  "Stop one or more modules. Each module name should be a symbol or string.

The following restarts are available:
  SKIP  --- Skip stopping the module.
  FORCE --- Force stopping even though it isn't active.
  RETRY --- Simply retry stopping."
  (dolist (module-name module-names)
    (setf module-name (to-module-name module-name))
    (with-simple-restart (skip "Skip stopping the module.")
      (let ((module (get-module module-name)))
        (assert (not (null module)) () "Module ~a not found!" module-name)
        (with-simple-restart (force "Force stopping the module.")
          (assert (not (not (active module))) () "Module ~a already stopped!" module-name))
        (v:info module-name "Stopping...")
        (loop until (with-simple-restart (retry "Retry stopping the module.")
                      (stop module)))
        module))))
