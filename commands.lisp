#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defvar *cmd-map* (make-hash-table))
(defvar *cmd-priority-array* (make-array 0))

(defclass command-handler ()
  ((%identifier :initarg :identifier :initform (error "Identifier required.") :accessor identifier)
   (%pattern :initarg :pattern :initform (error "Command pattern required.") :accessor pattern)
   (%arguments :initarg :arguments :initform () :accessor arguments)
   (%handler-function :initarg :handler-function :initform (error "Handler function required.") :accessor handler-function)
   (%priority :initarg :priority :initform 0 :accessor priority)
   (%docstring :initarg :docstring :initform NIL :accessor docstring))
  (:documentation ""))

(defun generate-command-priority-cache ()
  "Regenerates the command handler priority array.
Necessary to ensure proper dispatch order."
  (let ((array (make-array (hash-table-count *cmd-map*))))
    (loop for handler being the hash-values of *cmd-map*
          for i from 0
          do (setf (aref array i) handler))
    (setf array (sort array #'>))
    (v:trace :command "Rebuilding priority array.")
    (setf *cmd-priority-array* array)))

(defun command-handler (identifier)
  "Returns the COMMAND-HANDLER instance associated with the given IDENTIFIER."
  (gethash identifier *cmd-map*))

(defgeneric (setf command-handler) (command-handler identifier)
  (:documentation "Set a new COMMAND-HANDLER instance for a given IDENTIFIER.")
  (:method ((command-handler command-handler) (identifier symbol))
    (assert (eq identifier (identifier command-handler)) ()
            "The identifier of the COMMAND-HANDLER does not match (~a != ~a)" identifier (identifier command-handler))
    (setf (gethash identifier *cmd-map*) command-handler)
    (generate-command-priority-cache)))

(defun remove-command-handler (identifier)
  "Remove the COMMAND-HANDLER associated with the given IDENTIFIER."
  (assert (symbolp identifier) () "IDENTIFIER has to be a symbol.")
  (remhash identifier *cmd-map*)
  (generate-handler-priority-cache)
  identifier)

(defun set-command-function (identifier cmd-pattern arguments handler-function &key (priority (length cmd-pattern)) docstring)
  "Set a new handler function for a command pattern.

IDENTIFIER       --- A symbol identifying your handler.
CMD-PATTERN      --- A REGEX pattern to match the message with. Usually you'll
                     want to do something like \"^my-command (.*)\". Arguments are 
                     matched by the last regex group.
ARGUMENTS        --- A list of function arguments that the command should accept.
                     This lambda list can only contain &OPTIONAL and &REST.
HANDLER-FUNCTION --- The function object to dispatch the command to. Has to
                     accept as many arguments as ARGUMENTS specifies as well
                     as a first argument that will be bound to the command event.
PRIORITY         --- Either a key from *PRIORITY-NAMES* or a real setting the
                     priority of the handler. Higher priorities are served first.
                     Defaults to the length of CMD-PATTERN, which should be sane.
DOCSTRING        --- An optional documentation string"
  (assert (symbolp identifier) () "IDENTIFIER has to be a symbol.")
  (setf priority (gethash priority *priority-names* priority))
  (assert (realp priority) () "PRIORITY has to be a real or a symbol from *PRIORITY-NAMES*.")

  (when (gethash identifier *cmd-map*)
    (v:info :command "Redefining handler ~a" identifier))
  (setf (gethash identifier *cmd-map*)
        (make-instance 'command-handler
                       :identifier identifier :pattern pattern :arguments arguments
                       :handler-function handler-function :priority priority :docstring docstring))
  (generate-command-priority-cache)
  identifier)

(defun read-command (event)
  (dolist (prefix (config-tree :command :prefix))
    (when (string= prefix "$NICK$") 
      (setf prefix (format NIL "~a:" (nick *current-server*))))
    (when (and (> (length (message event)) (length prefix))
               (string= (message event) prefix :end1 (length prefix)))
      (dispatch (make-instance 'command-event
                               :server (server event)
                               :arguments (arguments event)
                               :prefix (prefix event)
                               :message (message (subseq (message event) (length prefix))))))))

(defun dispatch-command (event)
  )

(set-handler-function :command-reader 'events:privmsg-event #'read-command)
(set-handler-function :command-dispatcher 'events:command-event #'dispatch-command)
