#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defclass module ()
  ((%active :initform NIL :accessor active :allocation :class)
   (%handlers :initform (make-hash-table :test 'equal) :reader handlers :allocation :class)
   (%commands :initform (make-hash-table :test 'equal) :reader commands :allocation :class)
   (%groups :initform (make-hash-table) :reader groups :allocation :class))
  (:documentation "Base module class."))

(defmethod print-object (module stream)
  (format stream "<[~a]>" (class-name (class-of module))))

(defclass command ()
  ((%name :initform (error "Name required.") :initarg :name :accessor name)
   (%args :initform () :initarg :arguments :accessor cmd-args)
   (%func :initform (error "Function required.") :initarg :function :accessor cmd-fun)
   (%docu :initform NIL :initarg :documentation :accessor docu))
  (:documentation "Class for module commands."))

(defgeneric start (module)
  (:documentation "Start the module and activate it for use."))
(defgeneric stop (module)
  (:documentation "Stop the module and clean everything up."))
(defgeneric add-group (module group)
  (:documentation "Register a new group in the module."))
(defgeneric add-group-command (module group command args method &optional documentation)
  (:documentation "Add a new command to a group in the module."))
(defgeneric add-command (module command args method &optional documentation)
  (:documentation "Add a new command to the module."))
(defgeneric add-handler (module eventtype method)
  (:documentation "Add a new event handler to the module."))
(defgeneric dispatch (module event &key &allow-other-keys)
  (:documentation "Handle a new event and distribute it to the registered handler functions."))
(defgeneric get-command (module commandname)
  (:documentation "Get the command instance associated with the command name."))
(defgeneric get-group (module group-symbol)
  (:documentation "Return the hash-map defining the group."))
(defgeneric get-group-command (module group-symbol commandname)
  (:documentation "Return the function symbol to the group-command."))

(defmethod start :after ((module module))
  (setf (active module) T))

(defmethod start ((module module)))

(defmethod stop :before ((module module))
  (setf (active module) NIL))

(defmethod stop ((module module)))

(defmethod add-group ((module module) group)
  (unless (gethash group (groups module))
    (setf (gethash group (groups module)) (make-hash-table :test 'equal))))

(defmethod add-group-command ((module module) group command args method &optional documentation)
  (let ((command (string-downcase command)))
    (assert (not (null (gethash group (groups module)))))
    (setf (gethash command (gethash group (groups module)))
          (make-instance 'command :name command :arguments args :function method :documentation documentation))))

(defmethod add-command ((module module) command args method &optional documentation)
  (setf (gethash (string-downcase command) (commands module))
        (make-instance 'command :name command :arguments args :function method :documentation documentation)))

(defmethod add-handler ((module module) eventtype method)
  (setf (gethash (string-downcase eventtype) (handlers module)) method))

(defmethod dispatch ((module T) (event event) &key ignore-errors)
  (loop for module being the hash-values of *bot-modules*
     if (active module)
     do (when (if ignore-errors 
                  (ignore-errors (dispatch module event)) 
                  (dispatch module event))
          (return-from dispatch))))

(defmethod dispatch ((module module) (event event) &key)
  (let ((handler (gethash (string-downcase (class-name (class-of event))) (handlers module))))
    (when handler
      (funcall handler module event)
      NIL)))

(defmethod dispatch ((module module) (event command-event) &key)
  (let ((handler (gethash (command event) (commands module))))
    (when handler
      (funcall (cmd-fun handler) module event)
      T)))

(defmethod get-command ((module module) commandname)
  (gethash (string-downcase commandname) (commands module)))

(defmethod get-group ((module module) group-symbol)
  (gethash group-symbol (groups module)))

(defmethod get-group-command ((module module) group-symbol commandname)
  (let ((group (get-group module group-symbol)))
    (when group (gethash commandname group))))

(defun get-module (modulename)
  "Return a module by its keyword name."
  (gethash modulename *bot-modules*))

(defun package-symbol (package)
  (let ((name (package-name package)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))

(defun get-current-module (&optional (package *package*))
  (get (package-symbol package) :module))

(defun display-help (module group event)
  "Responds with help about a specific command or the entire group."
  (if (cmd-args event)
      (let ((group-command (get-group-command module group (first (cmd-args event)))))
        (when group-command
          (respond event "~a" (docu group-command))
          (respond event "USAGE: ~a ~a ~{~a~^ ~}" group (name group-command) (cmd-args group-command))))
      (progn
        (respond event (docu (get-command module group)))
        (respond event "Commands: ~{~a ~}" (hash-table-keys (get-group module group))))))

(defmacro define-module (name direct-superclasses direct-slots &body options)
  "Define a new module."
  (let ((instancesym (gensym "INSTANCE")))
    `(progn 
       (defclass ,name (module ,@direct-superclasses)
         ((%active :initform NIL :reader active :allocation :class)
          (%handlers :initform (make-hash-table :test 'equal) :reader handlers :allocation :class)
          (%commands :initform (make-hash-table :test 'equal) :reader commands :allocation :class)
          (%groups :initform (make-hash-table) :reader groups :allocation :class)
          ,@direct-slots)
         ,@options)
       (let ((,instancesym (make-instance ',name)))
         (setf (get (package-symbol *package*) :module) ,instancesym
               (gethash ,(intern (string-upcase name) "KEYWORD") *bot-modules*) ,instancesym)))))

(defmacro define-group (name &key (module `(get-current-module)) documentation)
  "Define a new command group for a module."
  (let ((documentation (if documentation (format NIL "~a (Group)" documentation) "(Group)")))
    (destructuring-bind (method &optional (command method)) (if (listp name) name (list name))
      `(progn
         (add-group ,module ',command)
         (add-command 
          ,module ',command '(&rest rest) 
          #'(lambda (module event)
              (let* ((subcmd (or (first (cmd-args event)) "help"))
                     (command (gethash subcmd (gethash ',command (groups module)))))
                (if command
                    (progn
                      (setf (command event) (format NIL ,(format NIL "~a ~~a" command) subcmd))
                      (setf (cmd-args event) (cdr (cmd-args event)))
                      (funcall (cmd-fun command) module event))
                    (progn
                      (v:debug :colleen "~a No method found for ~a!" module subcmd)
                      (respond event (fstd-message event :no-command))))))
          ,documentation)
         (define-command (,command help) (&rest args) (:documentation "Display command information." :module ,module :modulevar module)
           (declare (ignore args))
           (display-help module ',command event))))))

(defmacro define-command (name (&rest args) (&key authorization documentation (eventvar 'event) (module `(get-current-module)) (modulevar 'module)) &body body)
  "Define a new command for a module."
  (destructuring-bind (group &optional (name group n-s-p)) (if (listp name) name (list name))
    (unless n-s-p (setf group NIL))
    (let ((methodgens (gensym "METHOD"))
          (errgens (gensym "ERROR")))
      `(let ((,methodgens
              (lambda (,modulevar ,eventvar)
                (declare (ignorable ,modulevar))
                ,(when authorization
                       `(unless (auth-p (nick ,eventvar))
                          (error 'not-authorized :event ,eventvar)))
                (handler-case
                    (destructuring-bind (,@args) (cmd-args ,eventvar)
                      ,@body)
                  ((or sb-kernel::arg-count-error 
                    sb-kernel::defmacro-lambda-list-broken-key-list-error
                    end-of-file) (,errgens)
                    (declare (ignore ,errgens))
                   (error 'invalid-arguments :command ',name :argslist ',args))))))
         ,(if group
              `(add-group-command ,module ',group ',name ',args ,methodgens ,documentation)
              `(add-command ,module ',name ',args ,methodgens ,documentation))))))

(defmacro define-handler (event-type (&key (module `(get-current-module)) (modulevar 'module)) &body body)
  "Define a new event handler for a module."
  (destructuring-bind (event-type &optional (eventvar event-type)) (if (listp event-type) event-type (list event-type))
    `(add-handler 
      ,module ',event-type
      (lambda (,modulevar ,eventvar)
        (declare (ignorable ,modulevar))
        ,@body))))
