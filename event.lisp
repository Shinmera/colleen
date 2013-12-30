#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defun make-event (event-name server prefix arguments)
  "Makes an event instance from the given parameters."
  (let ((class (gethash event-name *event-map*)))
    (if class
        (make-instance class :server server :prefix prefix :arguments arguments)
        (v:warn (name server) "Inexistent event: ~a ~a ~a" event-name prefix arguments))))

(defclass event ()
  ((%server :initarg :server :reader server)
   (%prefix :initarg :prefix :reader prefix)
   (%args :initarg :arguments :reader arguments))
  (:documentation "Base event class."))

(defmacro arguments-bind ((&rest vars) expression &body body)
  "Destructuring-bind extension to make event parsing easier.
Any list ends with a &REST if not provided.
Alternatively to &REST you can bass a &STRING symbol that will parse the REST to one single string."
  (let ((ignores) (stringvar))
    ;; Change BODY to REST and remember var name.
    (let ((stringpos (position '&string vars)))
      (when stringpos
        (setf stringvar (nth (1+ stringpos) vars))
        (setf (nth stringpos vars) '&rest)))
    ;; Always add a REST
    (unless (find '&rest vars)
      (let ((gensym (gensym "REST")))
        (setf vars (append vars (list '&rest gensym)))
        (push gensym ignores)))
    ;; Filter out NILs
    (setf vars (mapcar #'(lambda (var) 
                           (if var var (let ((gensym (gensym)))
                                         (push gensym ignores)
                                         gensym))) vars))
    ;; Put together.
    `(destructuring-bind ,vars ,expression
       (declare (ignore ,@ignores))
       ,@(when stringvar (list `(setf ,stringvar (format NIL "~{~a~^ ~}" ,stringvar))))
       ,@body)))

(defmacro define-event (name event-or-code (&rest superclasses) &optional structure class-options)
  "Define a new event class for a given IRC event code.
NAME is the class-name to use for the event, preferably suffixed by -event.
EVENT-OR-CODE is the event symbol as in REPLY-CODES or the IRC event name or number.
SUPERCLASSES is a list of superclasses, defaulting to (EVENT)
STRUCTURE is a lambda-list of the arguments that are to be parsed for this event. See ARGUMENTS-BIND.
CLASS-OPTIONS are the other options that can be passed to DEFCLASS, such as :DOCUMENTATION and so on."
  (let ((eventvar (gensym "EVENT"))
        (restvar (gensym "REST"))
        (varlist (remove-if-not #'(lambda (var) (and var (not (find var '(&rest &string &optional))))) structure)))
    `(progn
       (defclass ,name (,@superclasses) 
         ,(loop for var in varlist
             collect `(,(intern (format NIL "%~a" var)) :initarg ,(intern (format NIL "~a" var) "KEYWORD") :initform NIL :reader ,var)) 
         (,@class-options))
       ,(when event-or-code
              `(setf (gethash ,event-or-code *event-map*) ',name))
       ,(when structure
          `(progn (defmethod initialize-instance :after ((,eventvar ,name) &rest ,restvar)
                    (declare (ignore ,restvar))
                    (arguments-bind (,@structure) (arguments ,eventvar)
                      ,@(loop for var in varlist
                           collect `(setf (slot-value ,eventvar ',(find-symbol (format NIL "%~a" var))) ,var))))
                  (defmethod print-object ((,eventvar ,name) stream)
                    (print-unreadable-object (,eventvar stream :type T)
                      (format stream ,(format NIL "~{~a: ~~a~^ ~}" varlist)
                              ,@(mapcar #'(lambda (var) `(,var ,eventvar)) varlist)))))))))

(defclass user-event (event)
    ((%username :initarg :username :reader username)
     (%hostmask :initarg :hostmask :reader hostmask)
     (%nickname :initarg :nick :reader nick))
    (:documentation "Events related to users."))

(defmethod initialize-instance :after ((event user-event) &rest rest)
  (declare (ignore rest))
  (cl-ppcre:register-groups-bind (nick username hostmask) (*user-regex* (prefix event))
    (setf (slot-value event '%username) username
          (slot-value event '%hostmask) hostmask
          (slot-value event '%nickname) nick))
  (unless (slot-boundp event '%nickname)
    (setf (slot-value event '%username) (prefix event)
          (slot-value event '%hostmask) (prefix event)
          (slot-value event '%nickname) (prefix event))))

(defmethod print-object ((event user-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "NICK: ~a USER: ~a HOST: ~a" (nick event) (username event) (hostmask event))))

(define-event channel-event NIL (user-event)
    (channel)
    (:documentation "Events for channel commands."))

(defmethod initialize-instance :after ((event channel-event) &rest rest)
  (declare (ignore rest))
  (with-slots ((channel %channel)) event
    (setf channel (first (arguments event)))
    (unless (char= (aref channel 0) #\#)
      (setf channel (nick event)))))

(defgeneric respond (event message &rest format-args)
  (:documentation "Respond to an event origin with the given message."))

(defmethod respond ((event user-event) message &rest format-args)
  (let ((message (apply #'format NIL message format-args)))
    (v:debug (name (server event)) "Replying to ~a: ~a" event message)
    (irc:privmsg (nick event) message :server (server event))))

(defmethod respond ((event channel-event) message &rest format-args)
  (let ((message (apply #'format NIL message format-args)))
    (v:debug (name (server event)) "Replying to ~a: ~a" event message)
    (irc:privmsg (channel event) message :server (server event))))

(defclass command-event (channel-event)
  ((%command :initarg :command :accessor command)
   (%cmd-args :initarg :cmd-args :accessor cmd-args))
  (:documentation "Event for commands."))

(defclass send-event (event) 
  ((%nick :initarg :nick :accessor nick)
   (%channel :initarg :channel :accessor channel)
   (%message :initarg :message :accessor message))
  (:documentation "Event for when the bot sends a message."))
