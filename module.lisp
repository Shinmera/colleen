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
(defgeneric dispatch (module event)
  (:documentation "Handle a new event and distribute it to the registered handler functions."))
(defgeneric get-group (module groupname)
  (:documentation "Return the hash-map defining the group."))
(defgeneric get-group-command (module groupname commandname)
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

(defmethod dispatch ((module module) (event event))
  (let ((handler (gethash (string-downcase (class-name (class-of event))) (handlers module))))
    (when handler
      (funcall handler module event))))

(defmethod dispatch ((module module) (event command-event))
  (let ((handler (gethash (command event) (commands module))))
    (when handler
      (funcall (cmd-fun handler) module event)
      T)))

(defmethod get-group ((module module) groupname)
  (gethash groupname (groups module)))

(defmethod get-group-command ((module module) groupname commandname)
  (let ((group (get-group module groupname)))
    (when group (gethash commandname group))))

(defun get-module (modulename)
  "Return a module by its keyword name."
  (gethash modulename *bot-modules*))

(defun display-help (module group event)
  "Responds with help about a specific command or the entire group."
  (if (cmd-args event)
      (let ((group-command (get-group-command module group (first (cmd-args event)))))
        (when group-command
          (respond event "~a" (docu group-command))
          (respond event "USAGE: ~a ~a ~{~a~^ ~}" group (name group-command) (cmd-args group-command))))
      (progn
        (respond event (docu (gethash group (commands module))))
        (respond event "Commands: ~{~a ~}" (hash-table-keys (get-group module group))))))

(defmacro define-module (name direct-superclasses direct-slots &body options)
  "Define a new module."
  `(progn 
     (defclass ,name (module ,@direct-superclasses)
       ((%active :initform NIL :reader active :allocation :class)
        (%handlers :initform (make-hash-table :test 'equal) :reader handlers :allocation :class)
        (%commands :initform (make-hash-table :test 'equal) :reader commands :allocation :class)
        (%groups :initform (make-hash-table) :reader groups :allocation :class)
        ,@direct-slots)
       ,@options)
     (setf (gethash ,(intern (string-upcase name) "KEYWORD") *bot-modules*)
           (make-instance ',name))))

(defmacro define-group (name module &key documentation)
  "Define a new command group for a module."
  (destructuring-bind (method &optional (command method)) (if (listp name) name (list name))
    (let ((instance `(get-module ,(find-symbol (string-upcase module) "KEYWORD"))))
      `(progn
         (add-group ,instance ',command)
         (add-command 
          ,instance ',command '(&rest rest) 
          #'(lambda (module event)
              (let* ((subcmd (or (first (cmd-args event)) "help"))
                     (command (gethash subcmd (gethash ',command (groups module)))))
                (if command
                    (progn
                      (setf (command event) (format NIL ,(format NIL "~a ~~a" command) subcmd))
                      (setf (cmd-args event) (cdr (cmd-args event)))
                      (funcall (cmd-fun command) module event))
                    (progn
                      (v:debug ,(find-symbol (string-upcase module) "KEYWORD") "No method found for ~a!" subcmd)
                      (respond event (fstd-message event :no-command))))))
          ,documentation)
         (define-command (,command help) ,module (&rest args) (:documentation "Display command information.")
           (declare (ignore args))
           (display-help ,module ',command event))))))

(defmacro define-command (name module (&rest args) (&key (eventvar 'event) authorization documentation) &body body)
  "Define a new command for a module."
  (destructuring-bind (group &optional (name group n-s-p)) (if (listp name) name (list name))
    (unless n-s-p (setf group NIL))
    (let ((instance `(get-module ,(find-symbol (string-upcase module) "KEYWORD")))
          (methodgens (gensym "METHOD"))
          (errgens (gensym "ERROR")))
      `(let ((,methodgens
              (lambda (,module ,eventvar)
                (declare (ignorable ,module))
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
              `(add-group-command ,instance ',group ',name ',args ,methodgens ,documentation)
              `(add-command ,instance ',name ',args ,methodgens ,documentation))))))

(defmacro define-handler (module (event-type &optional (eventvar event-type)) &body body)
  "Define a new event handler for a module."
  (destructuring-bind (class &optional (varname class) (instance `(get-module ,(find-symbol (string-upcase class) "KEYWORD")))) 
      (if (listp module) module (list module))
    `(add-handler 
      ,instance ',event-type
      (lambda (,varname ,eventvar)
        (declare (ignorable ,varname))
        ,@body))))
