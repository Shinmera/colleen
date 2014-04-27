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
(defvar *priority-names*
  (loop with map = (make-hash-table)
        for (name val) in '((:PREPROCESS 200) (:BEFORE 100)
                            (:MAIN 0) (:STANDARD 0) (:DEFAULT 0)
                            (:AFTER -100) (:POSTPROCESS -200))
        do (setf (gethash name map) val)
        finally (return map))
  "Hash table mapping arbitrary names to event priorities.
Defined by default are :PREPROCESS :BEFORE :MAIN :STANDARD
:DEFAULT :AFTER :POSTPROCESS.")

(defclass event-handler ()
  ((%event-type :initarg :event-type :initform 'event :accessor event-type)
   (%identifier :initarg :identifier :initform (error "Identifier required.") :accessor identifier)
   (%handler-function :initarg :handler-function :initform (error "Handler function required.") :accessor handler-function)
   (%priority :initarg :priority :initform 0 :accessor priority)
   (%docstring :initarg :docstring :initform NIL :accessor docstring))
  (:documentation "Container class representing an event handler."))

(defmethod print-object ((handler event-handler) stream)
  (print-unreadable-object (handler stream :type T)
    (print (identifier handler) stream))
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
  (setf priority (gethash priority *priority-names* priority))
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
  (:documentation "Dispatch an event instance through the event system.")
  (:method ((event event))
    (with-simple-restart (stop-event "Stop dispatching ~a" event)
      (loop for handler in (gethash (type-of event) *evt-priority-map*)
            until (cancelled event)
            do (with-simple-restart (skip-event "Skip dispatching to ~a" handler)
                 (with-repeating-restart (retry-event "Retry dispatching to ~a" handler)
                   (funcall (handler-function handler) event) T))))
    event))

(defmacro define-handler (event-type (&key (modulevar 'module) module-name (priority :MAIN) identifier) &body body)
  "Define an event handler for a module.

EVENT-TYPE  ::= TYPE | (TYPE NAME)
TYPE        --- The type of event class you want to handle.
NAME        --- A symbol for the variable name to bind the event to.
                By default the same as TYPE.
MODULEVAR   --- The symbol to bind the module instance to.
MODULE-NAME --- Your module identifier. Defaults to whatever 
                GET-CURRENT-MODULE-NAME returns.
PRIORITY    --- See SET-HANDLER-FUNCTION.
IDENTIFIER  --- See SET-HANDLER-FUNCTION. Defaults to a symbol made up
                of MODULE-NAME and TYPE.
BODY        ::= FORM*"
  (unless module-name (setf module-name (get-current-module-name)))
  (destructuring-bind (event-type &optional (event-var event-type)) (ensure-list event-type)
    (let ((auto-ident (format NIL "~a-~a" module-name event-type)))
      `(set-handler-function ,(or identifier
                                  (find-symbol auto-ident)
                                  (intern auto-ident))
                             ',event-type #'(lambda (,event-var)
                                              (let ((,modulevar (get-module ,module-name)))
                                                ,@body))
                             :priority ,priority))))
