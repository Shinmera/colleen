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
   (%scanner :accessor scanner)
   (%arguments :initarg :arguments :initform () :accessor arguments)
   (%handler-function :initarg :handler-function :initform (error "Handler function required.") :accessor handler-function)
   (%priority :initarg :priority :initform 0 :accessor priority)
   (%docstring :initarg :docstring :initform NIL :accessor docstring))
  (:documentation "Container class representing a command handler."))

(defmethod initialize-instance :after ((handler command-handler) &rest args)
  (declare (ignore args))
  (setf (scanner handler) (cl-ppcre:create-scanner (pattern handler) :case-insensitive-mode T)))

(defmethod print-object ((handler command-handler) stream)
  (print-unreadable-object (handler stream :type T)
    (print (identifier handler) stream))
  handler)

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
  (generate-command-priority-cache)
  identifier)

(defun set-command-function (identifier cmd-pattern handler-function &key (arguments () a-p) (priority (length cmd-pattern)) docstring)
  "Set a new handler function for a command pattern.

IDENTIFIER       --- A symbol identifying your handler.
CMD-PATTERN      --- A REGEX pattern to match the message with. Usually you'll
                     want to do something like \"^my-command (.*)\". Arguments are 
                     matched by the last regex group.
HANDLER-FUNCTION --- The function object to dispatch the command to. Has to
                     accept as many arguments as ARGUMENTS specifies as well
                     as a first argument that will be bound to the command event.
ARGUMENTS        --- A list of function arguments that the command should accept.
                     This lambda list can only contain &OPTIONAL and &REST.
                     If not provided, it will attempt to retrieve it through
                     SB-INTROSPECT:FUNCTION-LAMBDA-LIST or FUNCTION-LAMBDA-EXPRESSION.
                     If this fails, an error is thrown.
PRIORITY         --- Either a key from *PRIORITY-NAMES* or a real setting the
                     priority of the handler. Higher priorities are served first.
                     Defaults to the length of CMD-PATTERN, which should be sane.
DOCSTRING        --- An optional documentation string"
  (assert (symbolp identifier) () "IDENTIFIER has to be a symbol.")
  (setf priority (gethash priority *priority-names* priority))
  (assert (realp priority) () "PRIORITY has to be a real or a symbol from *PRIORITY-NAMES*.")

  (when (command-handler identifier)
    (v:info :command "Redefining handler ~a" identifier))
  (unless a-p
    (setf arguments (cdr #+sbcl (sb-introspect:function-lambda-list handler-function)
                         #+(and swank (not sbcl)) (swank-backend:arglist handler-function)
                         #-(or sbcl swank) (second (nth-value 2 (function-lambda-expression handler-function))))))
  (setf (command-handler identifier)
        (make-instance 'command-handler
                       :identifier identifier :pattern cmd-pattern :arguments arguments
                       :handler-function handler-function :priority priority :docstring docstring))
  identifier)

(defgeneric apropos-command-handler (handler)
  (:documentation "Returns a string describing the given command handler if it exists.")
  (:method ((name symbol))
    (when-let ((handler (command-handler name)))
      (apropos-command-handler handler)))

  (:method ((command string))
    (loop for handler across *cmd-priority-array*
          do (when (cl-ppcre:scan (scanner handler) command)
               (return-from apropos-command-handler
                 (apropos-command-handler handler)))))
  
  (:method ((handler command-handler))
    (format NIL "[Command Handler] ~s matching ~s with priority ~a expecting ~s~%~
                 ~:[No docstring available.~;Docstring: ~:*~a~]"
            (identifier handler) (pattern handler) (priority-name (priority handler)) (arguments handler) (docstring handler))))

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
(set-handler-function :command-reader 'events:privmsg-event #'read-command)

(defun lambda-keyword-p (symbol)
  (find symbol '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))
(defun flatten-lambda-list (lambda-list)
  (mapcar #'(lambda (a) (if (listp a) (car a) a)) lambda-list))
(defun extract-lambda-vars (lambda-list)
  (remove-if #'lambda-keyword-p (flatten-lambda-list lambda-list)))
(defun required-lambda-vars (lambda-list)
  (loop for i in lambda-list
        until (lambda-keyword-p i)
        collect i))

(defun arguments-match-p (lambda-list provided)
  (let ((num (length provided))
        (requireds (length (required-lambda-vars lambda-list))))
    (or (= num requireds)                                 ; Only as long as required, good.
        (and (> num requireds)                            ; But shorter is not allowed.
             (or (find '&rest lambda-list)                ; If we have &rest, any amount is good.
                 (and (find '&optional lambda-list)       ; Without &rest, &optional is possible.
                      (< num (length lambda-list))))))))  ; But then it cannot be longer than the amount of optionals.

(defun dispatch-command (event)
  (with-simple-restart (stop-command "Stop dispatching ~a" event)
    (loop for handler across *cmd-priority-array*
          until (cancelled event)
          do (with-simple-restart (skip-command "Skip dispatching to ~a" handler)
               (with-repeating-restart (rematch-command "Try matching the handler pattern again.")
                 (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings (scanner handler) (message event))
                   (when match
                     (v:trace :command "Event ~a matched ~a." event handler)
                     (let ((args (split-sequence #\Space (string-trim " " (aref groups (1- (length groups)))))))
                       (with-repeating-restart (recheck-command "Try checking the arguments again.")
                         (unless (arguments-match-p (cdr (arguments handler)) args)
                           (error 'invalid-arguments :argslist (cdr (arguments handler)) :command (identifier handler)))
                         (with-repeating-restart (retry-command "Retry dispatching to ~a")
                           (v:debug :command "Dispatching ~a to ~a." event handler)
                           (setf (cancelled event) T) ; Cancel by default
                           (apply (handler-function handler) event args)
                           (setf (handled event) T)
                           T))))))))))
(set-handler-function :command-dispatcher 'events:command-event #'dispatch-command)

(defun no-command-matched (event)
  (respond event (fstd-message event :no-command "No such command.")))
(set-command-function :no-command "" #'no-command-matched
                      :arguments () :priority :LAST
                      :docstring "Catchall command handler for when no command matched.")


(defclass group-handler (command-handler)
  ((%subcommands :initarg :subcommands :initform () :accessor subcommands))
  (:documentation ""))

(defmethod apropos-command-handler ((handler group-handler))
  (format NIL "[Command Group] ~s matching ~s~%~
               ~:[No docstring available.~;Docstring: ~:*~a~]~%~
               Commands in this group: ~{~a~^, ~}"
          (identifier handler) (pattern handler) (docstring handler) (subcommands handler)))

(defun escape-regex-symbols (string)
  (cl-ppcre:regex-replace-all "([\\\\\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}])" string '("\\" 0)))

(defmacro define-group (name &key documentation pattern)
  (unless pattern
    (setf pattern (format NIL "^~a(.*)" (escape-regex-symbols name))))
  `(set-command-function ',name ,pattern #'(lambda (event &rest args)
                                             (if args
                                                 (respond event (apropos-command-handler (message event)))
                                                 (respond event (apropos-command-handler (command-handler ',name)))))
                         :docstring ,documentation :priority :AFTER))

(defmacro define-command (name (&rest args) (&key authorization documentation (eventvar 'event) module-name (modulevar 'module) pattern priority) &body body)
  )
