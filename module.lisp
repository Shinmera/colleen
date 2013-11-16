#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defclass module ()
  ((%active :initform NIL :accessor active :allocation :class)
   (%handlers :initform (make-hash-table :test 'equal) :reader handlers :allocation :class)
   (%commands :initform (make-hash-table :test 'equal) :reader commands :allocation :class))
  (:documentation "Base module class."))

(defgeneric start (module)
  (:documentation "Start the module and activate it for use."))

(defmethod start :after ((module module))
  (setf (active module) T))

(defgeneric stop (module)
  (:documentation "Stop the module and clean everything up."))

(defmethod stop :before ((module module))
  (setf (active module) NIL))

(defgeneric add-command (module command method)
  (:documentation "Add a new command to the module."))

(defmethod add-command ((module module) command method)
  (setf (gethash (string-downcase command) (commands module)) method))

(defgeneric add-handler (module eventtype method)
  (:documentation "Add a new event handler to the module."))

(defmethod add-handler ((module module) eventtype method)
  (setf (gethash (string-downcase eventtype) (handlers module)) method))

(defgeneric dispatch (module event)
  (:documentation "Handle a new event and distribute it to the registered handler functions."))

(defmethod dispatch ((module module) (event event))
  (let ((handler (gethash (string-downcase (class-name (class-of event))) (handlers module))))
    (when handler
      (funcall handler module event))))

(defmethod dispatch ((module module) (event command-event))
  (let ((handler (gethash (command event) (commands module))))
    (when handler
      (funcall handler module event)
      T)))

(defmacro define-module (name direct-superclasses direct-slots &rest options)
  "Define a new module."
  `(progn 
     (defclass ,name (module ,@direct-superclasses)
       ((%active :initform NIL :reader active :allocation :class)
        (%handlers :initform (make-hash-table :test 'equal) :reader handlers :allocation :class)
        (%commands :initform (make-hash-table :test 'equal) :reader commands :allocation :class)
        ,@direct-slots)
       ,@options)
     (setf (gethash ',name *bot-modules*)
           (make-instance ',name))))

(defmacro define-command (name module (&rest args) (&key (eventvar 'event) authorization) &body body)
  "Define a new command for a module."
  (destructuring-bind (class &optional (varname class) (instance `(gethash ',class *bot-modules*))) (if (listp module) module (list module))
    `(progn
       (defmethod ,name ((,varname ,class) ,eventvar)
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
             (error 'invalid-arguments :command ',name :argslist ',args))))
       (add-command ,instance ',name #',name))))

(defmacro define-handler (name module (event-type eventvar) &body body)
  "Define a new event handler for a module."
  (destructuring-bind (class &optional (varname class) (instance `(gethash ',class *bot-modules*))) (if (listp module) module (list module))
    `(progn
       (defmethod ,name ((,varname ,class) ,eventvar)
         ,@body)
       (add-handler ,instance ',event-type #',name))))
