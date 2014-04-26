#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defclass module ()
  ((%active :initform NIL :accessor active :allocation :class)
   (%threads :initform (make-hash-table :test 'equalp) :accessor threads :allocation :class))
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
  (:method :around ((module module))
    (call-next-method)
    (setf (active module) T)
    module)

  (:method ((module module))))

(defgeneric stop (module)
  (:documentation "Stop the module and attempt to clean everything up.")
  (:method :around ((module module))
    (setf (active module) NIL)
    (loop for uid being the hash-keys of (threads module)
          for thread being the hash-values of (threads module)
          do (if (thread-alive-p thread)
                 (interrupt-thread thread #'(lambda () (error 'module-stop)))
                 (remhash uid (threads module))))
    (call-next-method)
    module)

  (:method ((module module))))

(defmacro with-module-thread (module &body thread-body)
  (let ((uidgens (gensym "UUID"))
        (modgens (gensym "MODULE"))
        (modnamegens (gensym "MODULE-NAME")))
    `(let* ((,uidgens (princ-to-string (uuid:make-v4-uuid)))
            (,modgens (get-module ,module))
            (,modnamegens (name ,modgens)))
       (setf (gethash ,uidgens (threads ,modgens))
             (make-thread #'(lambda ()
                              (handler-case
                                  ,@thread-body
                                (module-stop (err)
                                  (declare (ignore err))
                                  (v:debug ,modnamegens "Received module-stop condition, leaving thread ~a." ,uidgens))
                                (error (err)
                                  (v:severe ,modnamegens "Unexpected error at thread-level: ~a" err)
                                  (when *debugger*
                                    (invoke-debugger err))))
                              (v:trace ,modnamegens "Ending thread ~a." ,uidgens)
                              (remhash ,uidgens (threads ,modgens)))
                          :initial-bindings `((*current-server* . ,*current-server*)
                                              (*servers* . ,*servers*))))
       ,uidgens)))

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
          ,@(mapcar #'(lambda (slot) (append slot '(:allocation :class))) direct-slots))
         ,@options)
       (when (and (gethash ,keyname *bot-modules*)
                  (active (gethash ,keyname *bot-modules*)))
         (v:warn :colleen "Redefining started module ~a. Let's hope everything goes well..." ,keyname))
       (setf (get (package-symbol *package*) :module) ,keyname
             (gethash ,keyname *bot-modules*) (make-instance ',name)))))

(defun start-module (&rest module-names)
  "Start up one or more modules. Each module name should be a symbol or string."
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
  "Stop one or more modules. Each module name should be a symbol or string."
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
