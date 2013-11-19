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

(defgeneric start (module)
  (:documentation "Start the module and activate it for use."))

(defmethod start :after ((module module))
  (setf (active module) T))

(defmethod start ((module module)))

(defgeneric stop (module)
  (:documentation "Stop the module and clean everything up."))

(defmethod stop :before ((module module))
  (setf (active module) NIL))

(defmethod stop ((module module)))

(defgeneric add-group (module group)
  (:documentation "Register a new group in the module."))
(defgeneric add-group-command (module group command method)
  (:documentation "Add a new command to a group in the module."))
(defgeneric add-command (module command method)
  (:documentation "Add a new command to the module."))
(defgeneric add-handler (module eventtype method)
  (:documentation "Add a new event handler to the module."))
(defgeneric dispatch (module event)
  (:documentation "Handle a new event and distribute it to the registered handler functions."))
(defgeneric get-group (module groupname)
  (:documentation "Return the hash-map defining the group."))
(defgeneric get-group-command (module groupname commandname)
  (:documentation "Return the function symbol to the group-command."))

(defmethod add-group ((module module) group)
  (unless (gethash group (groups module))
    (setf (gethash group (groups module)) (make-hash-table :test 'equal))))

(defmethod add-group-command ((module module) group command method)
  (let ((command (string-downcase command)))
    (assert (not (null (gethash group (groups module)))))
    (setf (gethash command (gethash group (groups module))) method)))

(defmethod add-command ((module module) command method)
  (setf (gethash (string-downcase command) (commands module)) method))

(defmethod add-handler ((module module) eventtype method)
  (setf (gethash (string-downcase eventtype) (handlers module)) method))

(defmethod dispatch ((module module) (event event))
  (let ((handler (gethash (string-downcase (class-name (class-of event))) (handlers module))))
    (when handler
      (funcall handler module event))))

(defmethod dispatch ((module module) (event command-event))
  (let ((handler (gethash (command event) (commands module))))
    (when handler
      (funcall handler module event)
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
          (respond event (documentation group-command 'FUNCTION))))
      (progn
        (respond event (documentation (symbol-function group) 'FUNCTION))
        (respond event (format NIL "Commands: ~{~a ~}" (hash-table-keys (get-group module group)))))))

(defmacro define-module (name direct-superclasses direct-slots &rest options)
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

(defmacro define-group (name module (&key documentation))
  "Define a new command group for a module."
  (destructuring-bind (method &optional (command method)) (if (listp name) name (list name))
    (let ((method-help (intern (format NIL "~a-HELP" method))))
      (destructuring-bind (class &optional (instance `(get-module ,(find-symbol (string-upcase module) "KEYWORD")))) 
          (if (listp module) module (list module))
        `(progn
           (defgeneric ,method (,class event)
             (:documentation ,documentation))
           (defmethod ,method ((module ,class) event)
             (let* ((subcmd (or (first (cmd-args event)) "help"))
                    (method (gethash subcmd (gethash ',command (groups module)))))
               (if method
                   (progn
                     (setf (command event) (format NIL ,(format NIL "~a ~~a" command) subcmd))
                     (setf (cmd-args event) (cdr (cmd-args event)))
                     (funcall method module event))
                   (progn
                     (v:debug ,(find-symbol (string-upcase module) "KEYWORD") "No method found for ~a!" subcmd)
                     (respond event (fstd-message event :no-command))))))
           (add-group ,instance ',command)
           (add-command ,instance ',command #',method)
           (define-command (,method-help help) ,module (&rest args) (:group ',command :documentation "Display command information.")
             (declare (ignore args))
             (display-help ,module ',command event)))))))

(defmacro define-command (name module (&rest args) (&key (eventvar 'event) authorization group documentation) &body body)
  "Define a new command for a module."
  (let ((documentation (format NIL "~@[~a ~]ARGUMENTS: ~a" documentation args)))
    (destructuring-bind (method &optional (command method)) (if (listp name) name (list name))
      (destructuring-bind (class &optional (varname class) (instance `(get-module ,(find-symbol (string-upcase module) "KEYWORD")))) 
          (if (listp module) module (list module))
        `(progn
           (defgeneric ,method (,class event)
             (:documentation ,documentation))
           (defmethod ,method ((,varname ,class) ,eventvar)
             ,(when authorization
                    `(unless (auth-p (nick ,eventvar))
                       (error 'not-authorized :event ,eventvar)))
             (handler-case
                 (destructuring-bind (,@args) (cmd-args ,eventvar)
                   ,@body)
               ((or sb-kernel::arg-count-error 
                 sb-kernel::defmacro-lambda-list-broken-key-list-error
                 end-of-file) (err)
                 (declare (ignore err))
                (error 'invalid-arguments :command ',command :argslist ',args))))
           ,(if group
                `(add-group-command ,instance ,group ',command #',method)
                `(add-command ,instance ',command #',method)))))))

(defmacro define-handler (name module (event-type eventvar) &body body)
  "Define a new event handler for a module."
  (destructuring-bind (class &optional (varname class) (instance `(get-module ,(find-symbol (string-upcase module) "KEYWORD")))) 
      (if (listp module) module (list module))
    `(progn
       (defgeneric ,name (,class event))
       (defmethod ,name ((,varname ,class) ,eventvar)
         ,@body)
       (add-handler ,instance ',event-type #',name))))
