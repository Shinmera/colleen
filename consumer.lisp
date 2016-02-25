#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defclass consumer-class (standard-class)
  ((direct-handlers :initform () :accessor direct-handlers)
   (handlers :initform () :accessor handlers)
   (instances :initform () :accessor instances)))

(defmethod c2mop:validate-superclass ((class consumer-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass consumer-class))
  T)

(defmethod c2mop:validate-superclass ((class consumer-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class consumer-class) (superclass consumer-class))
  T)

(defun cascade-handler-changes (class)
  (let ((handlers (copy-list (direct-handlers class))))
    (loop for superclass in (c2mop:class-direct-superclasses class)
          when (c2mop:subclassp superclass 'consumer)
          do (loop for handler in (handlers superclass)
                   unless (find (name handler) handlers :key #'name)
                   do (push handler handlers)))
    (setf (handlers class) handlers))
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (c2mop:subclassp sub-class 'consumer-class)
                  (c2mop:class-finalized-p sub-class))
        do (cascade-handler-changes sub-class)))

(defmethod c2mop:finalize-inheritance :after ((class consumer-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (cascade-handler-changes class))

(defmethod (setf handlers) :after (handlers (class consumer-class))
  (setf (instances class)
        (loop for pointer in (instances class)
              for consumer = (trivial-garbage:weak-pointer-value pointer)
              when consumer
              collect (prog1 pointer 
                        (reinitialize-handlers consumer (handlers class))))))

(defmethod (setf direct-handlers) :after (handlers (class consumer-class))
  (c2mop:finalize-inheritance class))

(defun update-handler (handler class-ish)
  (let ((class (etypecase class-ish
                 (consumer-class class-ish)
                 (consumer (class-of class-ish))
                 (symbol (find-class class-ish)))))
    (update-list handler (direct-handlers class) :key #'name)))

(defclass consumer (named-entity)
  ((handlers :initform () :accessor handlers)
   (cores :initform () :accessor cores))
  (:metaclass consumer-class))

(defmethod initialize-instance :after ((consumer consumer) &key)
  (push (trivial-garbage:make-weak-pointer consumer) (instances (class-of consumer)))
  (dolist (handler (handlers (class-of consumer)))
    (push (instantiate-handler handler consumer) (handlers consumer))))

(defmethod reinitialize-handlers ((consumer consumer) handlers)
  (v:info :colleen.core.consumer "~a updating handlers." consumer)
  (let ((cores (cores consumer)))
    ;; Deregister
    (remove-consumer consumer cores)
    ;; Rebuild
    (stop consumer)
    (setf (handlers consumer) ())
    (dolist (handler handlers)
      (push (instantiate-handler handler consumer) (handlers consumer)))
    (start consumer)
    ;; Reregister
    (add-consumer consumer cores)))

(defmethod add-consumer :after ((consumer consumer) (core core))
  (register-handler (handlers consumer) core)
  (push core (cores consumer)))

(defmethod remove-consumer ((consumer consumer) (everywhere (eql T)))
  (dolist (core (cores consumer))
    (remove-consumer consumer core)))

(defmethod remove-consumer :after ((consumer consumer) (core core))
  (deregister-handler (handlers consumer) core)
  (setf (cores consumer) (remove core (cores consumer))))

(defmethod start ((consumer consumer))
  (start (handlers consumer))
  consumer)

(defmethod stop ((consumer consumer))
  (stop (handlers consumer))
  consumer)

(defclass abstract-handler ()
  ((target-class :initarg :target-class :accessor target-class)
   (options :initarg :options :accessor options)
   (name :initarg :name :accessor name))
  (:default-initargs
   :target-class 'queued-handler
   :options ()))

(defmethod initialize-instance :after ((handler abstract-handler) &rest args &key &allow-other-keys)
  (let ((class (getf args :target-class)))
    (when class (setf (target-class handler) class))
    (setf (options handler) (deeds::removef args :target-class :options))))

(defmethod instantiate-handler ((handler abstract-handler) (consumer consumer))
  (let* ((options (options handler))
         (filter (getf options :filter))
         (delivery (getf options :delivery-function))
         (match (getf options :match-consumer)))
    ;; Extend filter to match consumer.
    (when match
      (if (eql match T)
          (setf filter `(and (eq ,consumer consumer) ,filter))
          (setf filter `(and (eq ,consumer ,match) ,filter))))
    (apply #'make-instance
           (target-class handler)
           :delivery-function (lambda (event) (funcall delivery consumer event))
           :filter filter
           (deeds::removef options :delivery-function :filter :match-consumer))))

(defmacro define-handler ((consumer name event-type) args &body body)
  (destructuring-bind (compvar event &rest args) args
    (multiple-value-bind (options body) (deeds::parse-into-kargs-and-body body)
      (let ((class (or (getf options :class) 'queued-handler))
            (options (deeds::removef options :class)))
        `(progn
           (update-handler
            (make-instance
             'abstract-handler
             :target-class ',class
             :name ',name
             :event-type ',event-type
             :delivery-function (lambda (,compvar ,event)
                                  (declare (ignorable ,compvar ,event))
                                  (with-origin (',name)
                                    (with-fuzzy-slot-bindings ,args (,event ,event-type)
                                      ,@body)))
             ,@options)
            ',consumer)
           (list ',consumer ',name))))))

(defmacro define-consumer (name direct-superclasses direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'consumer)))
    (push 'consumer direct-superclasses))
  (unless (find :metaclass options :key #'first)
    (push `(:metaclass consumer-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
