#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defvar *cmd-map* (make-hash-table)
  "Hash table mapping command identifiers to command-handler instances.")
(defvar *cmd-priority-array* (make-array 0)
  "Priority sorted array for the command dispatching.

You should not modify this unless you know what you're doing as this
array is overwritten completely whenever GENERATE-COMMAND-PRIORITY-CACHE is invoked.")

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
    (format stream "~s" (identifier handler)))
  handler)

(defun generate-command-priority-cache ()
  "Regenerates the command handler priority array.
Necessary to ensure proper dispatch order."
  (let ((array (make-array (hash-table-count *cmd-map*))))
    (loop for handler being the hash-values of *cmd-map*
          for i from 0
          do (setf (aref array i) handler))
    (setf array (sort array #'> :key #'priority))
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

(defun set-command-function (identifier cmd-pattern handler-function &key (arguments () a-p) (priority (length cmd-pattern)) docstring (class 'command-handler))
  "Set a new handler function for a command pattern.

IDENTIFIER       --- A symbol identifying your handler.
CMD-PATTERN      --- A REGEX pattern to match the message with. Usually you'll
                     want to do something like \"^my-command(.*)\". Arguments are 
                     matched by the last regex group.
HANDLER-FUNCTION --- The function object to dispatch the command to. Has to
                     accept as many arguments as ARGUMENTS specifies as well
                     as a first argument that will be bound to the command event.
ARGUMENTS        --- A list of function arguments that the command should accept.
                     This lambda list can only contain &OPTIONAL and &REST.
                     If not provided, it will attempt to retrieve it through
                     FUNCTION-ARGUMENTS.
PRIORITY         --- Either a key from *PRIORITY-NAMES* or a real setting the
                     priority of the handler. Higher priorities are served first.
                     Defaults to the length of CMD-PATTERN, which should be sane.
DOCSTRING        --- An optional documentation string
CLASS            --- The class to make an instance of. Has to be subclass of 
                     COMMAND-HANDLER."
  (assert (symbolp identifier) () "IDENTIFIER has to be a symbol.")
  (setf priority (gethash priority *priority-names* priority))
  (assert (realp priority) () "PRIORITY has to be a real or a symbol from *PRIORITY-NAMES*.")

  (when (command-handler identifier)
    (v:debug :command "Redefining handler ~a" identifier))
  (unless a-p
    (setf arguments (cdr (function-arguments handler-function))))
  (setf (command-handler identifier)
        (make-instance class
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
    (let ((*print-pretty* NIL))
      (format NIL "[Command Handler] ~s matching ~s with priority ~s expecting ~a~%~
                 ~:[No docstring available.~;Docstring: ~:*~a~]"
              (identifier handler) (pattern handler) (priority-name (priority handler))
              (lambda-list->string (arguments handler)) (docstring handler)))))

(defun command-p (message)
  "If the message starts with a configured prefix and thus matches as a command,
the matched prefix is returned and otherwise NIL."
  (loop for prefix in (bot-config :command :prefix)
        do (when (string= prefix "$NICK$") 
             (setf prefix (format NIL "~a:" (nick *current-server*))))
        when (and (> (length message) (length prefix))
                  (string= message prefix :end1 (length prefix)))
          do (return prefix)
        finally (return NIL)))

(defun read-command (event)
  "Tries to read a command from a PRIVMSG-EVENT by matching the prefixes as defined in the config.
If a match occurs, a fitting COMMAND-EVENT is generated and dispatched."
  (let ((prefix (command-p (message event))))
    (when prefix
      (handler-bind ((not-authorized
                       #'(lambda (err)
                           (v:warn (name (server event)) "~a" err)
                           (respond event (fstd-message event :not-authorized))
                           (invoke-restart 'skip-handler)))
                     (invalid-arguments
                       #'(lambda (err)
                           (v:warn (name (server event)) "~a" err)
                           (respond event "Invalid arguments. Expected lambda-list: ~a"
                                    (lambda-list->string (expected err)))
                           (invoke-restart 'skip-handler))))
        (dispatch (make-instance 'command-event
                                 :server (server event)
                                 :arguments (arguments event)
                                 :prefix (prefix event)
                                 :message (subseq (message event) (length prefix))))))))
(set-handler-function :command-reader 'events:privmsg-event #'read-command)

(defmacro do-matching-command-handlers ((command-signature handlervar) &body body)
  "Applies BODY for each matching COMMAND-HANDLER, binding it to HANDLERVAR."
  (let ((signaturevar (gensym "COMMAND-SIGNATURE")))
    `(let ((,signaturevar ,command-signature))
       (loop for ,handlervar across *cmd-priority-array*
             when (cl-ppcre:scan (scanner ,handlervar) ,signaturevar)
               do ,@body))))

(defun arguments-match-p (lambda-list provided)
  "Returns T if it is possibly to pass the PROVIDED list of arguments to the LAMBDA-LIST.
Only cares about required, optional and rest args."
  (let ((num (length provided))
        (requireds (length (required-lambda-vars lambda-list))))
    (or (= num requireds)                                 ; Only as long as required, good.
        (and (> num requireds)                            ; But shorter is not allowed.
             (or (find '&rest lambda-list)                ; If we have &rest, any amount is good.
                 (and (find '&optional lambda-list)       ; Without &rest, &optional is possible.
                      (< num (length lambda-list))))))))  ; But then it cannot be longer than the amount of optionals.

(defgeneric dispatch-command (event)
  (:documentation "Dispatches an event to command handlers.

Unless specifically rebound, for command events the following restarts are available:
  STOP-COMMAND      --- Stops dispatching completely and returns.
  SKIP-HANDLER      --- Skips the current handler.
  REMATCH-HANDLER   --- Attempts to rematch the command with the handler's pattern.
  RECHECK-ARGUMENTS --- Rechecks the command arguments with the handler's arguments list.
  RETRY-HANDLER     --- Attempts invoking the handler again.")
  (:method ((event command-event))
    (with-simple-restart (stop-command "Stop dispatching ~a" event)
      (loop for handler across *cmd-priority-array*
            until (cancelled event)
            do (with-simple-restart (skip-handler "Skip dispatching to ~a" handler)
                 (with-repeating-restart (rematch-handler "Try matching the handler pattern again.")
                   (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings (scanner handler) (message event))
                     (when match
                       (v:trace :command "Event ~a matched ~a." event handler)
                       (setf (cancelled event) T) ; Cancel by default
                       (let ((args (split-sequence #\Space (string-trim " " (aref groups (1- (length groups)))) :remove-empty-subseqs T)))
                         (with-repeating-restart (recheck-arguments "Try checking the arguments again.")
                           (unless (arguments-match-p (arguments handler) args)
                             (error 'invalid-arguments :argslist args :expected (arguments handler) :command (identifier handler)))
                           (with-repeating-restart (retry-handler "Retry dispatching to ~a" handler)
                             (v:debug :command "Dispatching ~a to ~a." event handler)
                             (apply (handler-function handler) event args)
                             (setf (handled event) T)
                             T))))
                     T)))))))
(set-handler-function :command-dispatcher 'events:command-event #'dispatch-command)

(defun no-command-matched (event &rest args)
  "Catchall command handler for when no command matched."
  (declare (ignore args))
  (respond event (fstd-message event :no-command "No such command.")))
(set-command-function :no-command "(.*)" #'no-command-matched
                      :arguments '(&rest args) :priority :LAST
                      :docstring (documentation 'no-command-matched 'function))

(defun simulate-command (command-string &optional (output-stream *standard-output*))
  "Simulates passing a command.

This generates a GENERATED-COMMAND-EVENT with the given COMMAND-STRING and OUTPUT-STREAM.
The event will use the :NULL server, CL-USER!CL-USER@COLLEEN user ident and the INTERNAL channel."
  (dispatch-command (make-instance 'generated-command-event
                                   :message command-string :output-stream output-stream
                                   :server (get-server :null) :prefix "CL-USER!CL-USER@COLLEEN" :arguments '("INTERNAL"))))

(defun relay-command (event new-command)
  "Relays a command-event to a new command.

This generates a new COMMAND-EVENT with the given NEW-COMMAND as message.
The nick, username, etc. will be carried over from the event."
  (dispatch-command (make-instance 'command-event
                                   :message new-command :channel (channel event)
                                   :nick (nick event) :hostmask (hostmask event)
                                   :username (username event) :arguments (arguments event)
                                   :prefix (prefix event) :server (server event))))

(defclass group-handler (command-handler)
  ((%subcommands :initarg :subcommands :initform () :accessor subcommands))
  (:documentation "Special command handler class for command groups."))

(defmethod apropos-command-handler ((handler group-handler))
  (let ((*print-pretty* NIL))
    (format NIL "[Command Group] ~s matching ~s~%~
               ~:[No docstring available.~;Docstring: ~:*~a~]~%~
               Commands in this group: ~{~a~^, ~}"
            (identifier handler) (pattern handler) (docstring handler) (subcommands handler))))

(defun group-command-handler (event &rest args)
  (declare (ignore args))
  (let ((handler (loop for handler across *cmd-priority-array*
                       do (when (cl-ppcre:scan (scanner handler) (message event))
                            (return handler)))))
    (if handler
        (typecase handler
          (group-handler
           (respond event "!! Unknown command. Closest match: [Group] ~a -- ~
                            ~:[No docstring available.~;~:*~a~]~%~
                            Commands in this group: ~:[None~;~:*~{~a~^, ~}~]"
                    (identifier handler) (docstring handler) (subcommands handler)))
          (command-handler
           (respond event "!! Unknown command. Closest match: [Command] ~a -- ~
                            ~:[No docstring available.~;~:*~a~]~%~
                            Arguments: ~a"
                    (identifier handler) (docstring handler) (arguments handler))))
        (respond event "?? Something went very wrong."))))

(defmacro define-group (name &key documentation pattern subcommands)
  "Defines a new group with the given NAME.

NAME          --- A symbol dictating the group's identifier.
DOCUMENTATION --- Optional docstring.
PATTERN       --- The pattern to match the command with. Defaults
                  to ^NAME(.*)
SUBCOMMANDS   --- A list of subcommand identifiers that this group 
                  is supposedly providing.

See SET-COMMAND-FUNCTION"
  (assert (symbolp name) () "NAME has to be a symbol.")
  (unless pattern
    (setf pattern (format NIL "^~a(.*)" (escape-regex-symbols (string name)))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-command-function ',name ,pattern #'group-command-handler
                           :docstring ,documentation :priority :AFTER :class 'group-handler)
     ,(when subcommands
        `(setf (subcommands (command-handler ',name)) ,subcommands))))

(defmacro define-command (name (&rest args) (&key authorization documentation (eventvar 'event) module-name (modulevar 'module) pattern priority (threaded T)) &body body)
  "Defines a new command with the given NAME.

NAME          ::= IDENTIFIER | (GROUP IDENTIFIER)
IDENTIFIER    --- A symbol identifying your command.
GROUP         --- The command group to bind the command to.
ARGS          --- A list of arguments the command expects. This is a
                  reduced lambda-list that can only accept required,
                  optional and rest args. See SET-COMMAND-FUNCTION.
AUTHORIZATION --- If T, the command issuing NICK is checked with AUTH-P.
                  If the test fails, a NOT-AUTHORIZED error is raised.
DOCUMENTATION --- An optional docstring.
EVENTVAR      --- Symbol to bind the event variable to.
MODULE-NAME   --- An optional name to activate module convenience bindings.
                  Defaults to GET-CURRENT-MODULE-NAME when unset.
MODULEVAR     --- The symbol to bind the module instance to, if at all.
IDENTIFIER    --- See SET-COMMAND-FUNCTION. Defaults to a symbol made up
                  of MODULE-NAME and TYPE.
PATTERN       --- The pattern to match the command with. Defaults to
                  ^NAME(.*)
PRIORITY      --- See SET-COMMAND-FUNCTION.
THREADED      --- If a MODULE is found and bound to this handler, setting
                  this to T will execute the body in a module thread with
                  the module lock held. 
                  See WITH-MODULE-THREAD, WITH-MODULE-LOCK.
BODY          ::= FORM*"
  (assert (or (symbolp name) (listp name)) () "NAME has to be a symbol or a list of symbols.")
  (unless pattern
    (setf pattern (if (listp name)
                      (format NIL "^~{~a~^ ~}(.*)" (mapcar #'(lambda (a) (escape-regex-symbols (string a))) name))
                      (format NIL "^~a(.*)" (escape-regex-symbols (string name))))))
  (unless module-name
    (setf module-name (get-current-module-name)))
  (let* ((funcsym (gensym "FUNCTION"))
         (declarations (loop for form in body
                             until (or (not (listp form)) (not (eq (first form) 'declare)))
                             collect (pop body)))
         (body `(handler-bind ((error #'(lambda (err)
                                          (let ((*print-pretty* NIL))
                                            (respond ,eventvar "!! Unexpected error: ~a" err)))))
                  ,@body)))
    (flet ((mksymb (list)
             (let ((name (format NIL "~{~a~^ ~}" list)))
               (or (find-symbol name) (intern name)))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (let ((,funcsym #'(lambda (,eventvar ,@args)
                             (declare (ignorable ,eventvar))
                             ,@declarations
                             ,@(when authorization
                                 `((unless (auth-p (nick ,eventvar))
                                     (error 'not-authorized :event ,eventvar))))
                             ,(if module-name
                                  `(with-module (,modulevar ,module-name)
                                     (when (active ,modulevar)
                                       (with-module-storage (,modulevar)
                                         ,(if threaded
                                              `(with-module-thread (,modulevar)
                                                 (with-module-lock (,modulevar)
                                                   ,body))
                                              body))))
                                  body))))
           ,(if (listp name)
                (let ((group (car name))
                      (name (mksymb name)))
                  `(progn
                     ,(if (command-handler group)
                          `(pushnew ',name (subcommands (command-handler ',group)))
                          `(progn (warn 'implicit-group-definition :group ',group)
                                  (define-group ,group :subcommands (list ',name))))
                     (set-command-function ',name ,pattern ,funcsym :arguments ',args ,@(when priority `(:priority ,priority)) :docstring ,documentation)))
                `(set-command-function ',name ,pattern ,funcsym :arguments ',args ,@(when priority `(:priority ,priority)) :docstring ,documentation)))))))
