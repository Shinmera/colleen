#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defvar *evt-map* (make-hash-table)
  "Hash table mapping event identifiers to EVENT-HANDLER instances.")
(defvar *evt-priority-map* (make-hash-table)
  "Hash table mapping event classes to a list of event handlers ordered by their priorities. 

You should not modify this yourself unless you know what you're doing as this
map is overwritten completely whenever GENERATE-HANDLER-PRIORITY-CACHE is invoked.")

(defclass event-handler ()
  ((%event-type :initarg :event-type :initform 'event :accessor event-type)
   (%identifier :initarg :identifier :initform (error "Identifier required.") :accessor identifier)
   (%handler-function :initarg :handler-function :initform (error "Handler function required.") :accessor handler-function)
   (%priority :initarg :priority :initform 0 :accessor priority)
   (%docstring :initarg :docstring :initform NIL :accessor docstring))
  (:documentation "Container class representing an event handler."))

(defmethod print-object ((handler event-handler) stream)
  (print-unreadable-object (handler stream :type T)
    (format stream "~s" (identifier handler)))
  handler)

(defun generate-handler-priority-cache (&optional (handler-map *evt-map*))
  "Regenerates the event handler priority map.
Necessary to ensure proper event handling order."
  (let ((priority-map (make-hash-table)))
    ;; Rebuild
    (loop for handler being the hash-values of handler-map
          do (if (gethash (event-type handler) priority-map)
                 (setf (gethash (event-type handler) priority-map) (list handler))
                 (push handler (gethash (event-type handler) priority-map))))
    ;; Sort
    (loop for event-type being the hash-keys of priority-map
          for handlers being the hash-values of priority-map
          do (setf (gethash event-type priority-map)
                   (sort handlers #'> :key #'priority)))
    (v:trace :event "Rebuilding priority map.")
    (setf *evt-priority-map* priority-map)))

(defun event-handler (identifier)
  "Returns the EVENT-HANDLER instance associated with the given IDENTIFIER."
  (gethash identifier *evt-map*))

(defgeneric (setf event-handler) (event-handler identifier)
  (:documentation "Set a new EVENT-HANDLER instance for a given IDENTIFIER.")
  (:method ((event-handler event-handler) (identifier symbol))
    (assert (eq identifier (identifier event-handler)) ()
            "The identifier of the EVENT-HANDLER does not match (~a != ~a)" identifier (identifier event-handler))
    (setf (gethash identifier *evt-map*) event-handler)
    (generate-handler-priority-cache)))

(defun remove-event-handler (identifier)
  "Remove the EVENT-HANDLER associated with the given IDENTIFIER."
  (assert (symbolp identifier) () "IDENTIFIER has to be a symbol.")
  (remhash identifier *evt-map*)
  (generate-handler-priority-cache)
  identifier)

(defun set-handler-function (identifier event-class function &key (priority :MAIN) docstring)
  "Set a new handler function for an event class.

IDENTIFIER  --- A symbol identifying your handler.
EVENT-CLASS --- The type of event class you want to handle.
FUNCTION    --- The function object to dispatch to on event handling. Has to
                accept exactly one argument, the event to handle.
PRIORITY    --- Either a key from *PRIORITY-NAMES* or a real setting the
                priority of the handler. Higher priorities are served first.
DOCSTRING   --- An optional documentation string."
  (assert (symbolp identifier) () "IDENTIFIER has to be a symbol.")
  (setf priority (priority-num priority))
  (assert (realp priority) () "PRIORITY has to be a real or a symbol from *PRIORITY-NAMES*.")
  (assert (functionp function) () "FUNCTION has to be a function object.")
  
  (when (gethash identifier *evt-map*)
    (v:info :event "Redefining handler ~a" identifier))
  (setf (event-handler identifier)
        (make-instance 'event-handler
                       :event-type event-class :priority priority :identifier identifier
                       :handler-function function :docstring docstring))
  identifier)

(defgeneric dispatch (event)
  (:documentation "Dispatch an event instance through the event system.

Unless specifically rebound, for standard events the following restarts are available:
  STOP-EVENT    --- Stops dispatching completely and returns.
  SKIP-HANDLER  --- Skips the current handler.
  RETRY-HANDLER --- Attempts invoking the handler again.")
  (:method ((event event))
    (with-simple-restart (stop-event "Stop dispatching ~a" event)
      (loop for handler in (gethash (type-of event) *evt-priority-map*)
            until (cancelled event)
            do (with-simple-restart (skip-handler "Skip dispatching to ~a" handler)
                 (with-repeating-restart (retry-handler "Retry dispatching to ~a" handler)
                   (funcall (handler-function handler) event)
                   (setf (dispatched event) T)
                   T))))
    event))

(defmacro define-handler (event-type (&key (module-name NIL m-p) (modulevar 'module) identifier (priority :MAIN) (threaded T)) &body body)
  "Define an event handler for a module.

EVENT-TYPE  ::= TYPE | (TYPE NAME)
TYPE        --- The type of event class you want to handle.
NAME        --- A symbol for the variable name to bind the event to.
                By default the same as TYPE.
MODULE-NAME --- An optional name to activate module convenience bindings.
                Defaults to GET-CURRENT-MODULE-NAME when unset.
MODULEVAR   --- The symbol to bind the module instance to, if at all.
IDENTIFIER  --- See SET-HANDLER-FUNCTION. Defaults to a symbol made up
                of MODULE-NAME and TYPE.
PRIORITY    --- See SET-HANDLER-FUNCTION.
THREADED    --- If a MODULE is found and bound to this handler, setting
                this to T will execute the body in a module thread with
                the module lock held. 
                See WITH-MODULE-THREAD, WITH-MODULE-LOCK.
BODY        ::= FORM*"
  (when (and (not m-p) (not module-name))
    (setf module-name (get-current-module-name)))
  (destructuring-bind (event-type &optional (event-var event-type)) (ensure-list event-type)
    (let ((auto-ident (format NIL "~a-~a" module-name event-type))
          (funcsym (gensym "FUNCTION"))
          (declarations (loop for form in body
                              until (or (not (listp form)) (not (eq (first form) 'declare)))
                              collect (pop body))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (let ((,funcsym #'(lambda (,event-var)
                             ,@declarations
                             ,(if module-name
                                  `(with-module (,modulevar ,module-name)
                                     (declare (ignorable ,modulevar))
                                     ,(if threaded
                                          `(with-module-thread (,modulevar)
                                             (with-module-lock (,modulevar)
                                               ,@body))
                                          `(progn ,@body)))
                                  `(progn ,@body)))))
           (set-handler-function ',(or identifier
                                       (find-symbol auto-ident)
                                       (intern auto-ident))
                                 ',event-type ,funcsym
                                 :priority ,priority))))))

(defgeneric apropos-event-handler (handler)
  (:documentation "Returns a string describing the given event handler if it exists.")
  (:method ((name symbol))
    (when-let ((handler (event-handler name)))
      (apropos-event-handler handler)))
  
  (:method ((handler event-handler))
    (format NIL "[Event Handler] ~s for ~a with priority ~a~%~
                 ~:[No docstring available.~;Docstring: ~:*~a~]"
            (identifier handler) (event-type handler) (priority-name (priority handler)) (docstring handler))))
