#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defun make-event (event-name server prefix arguments)
  (let ((class (gethash event-name *event-map*)))
    (if class
        (make-instance class :server server :prefix prefix :arguments arguments)
        (v:warn (name server) "Inexistent event: ~a" event-name))))

(defclass event ()
  ((%server :initarg :server :reader server)
   (%prefix :initarg :prefix :reader prefix)
   (%args :initarg :arguments :reader arguments))
  (:documentation "Base event class."))

(defmacro arguments-bind ((&rest vars) expression &body body)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NON REPLY
(define-event ping-event :PING (event) 
    (server1 &optional server2)
    (:documentation "Event on a PING request."))

(define-event pong-event :PONG (event) 
    (server1 &optional server2)
    (:documentation "Event on a PONG request."))

(define-event nick-event :NICK (user-event)
    (old-nick)
    (:documentation "Event on nick change."))

(define-event privmsg-event :PRIVMSG (channel-event)
    (channel message)
    (:documentation "Privmsg event."))

(defmethod initialize-instance :after ((event privmsg-event) &rest rest)
  (declare (ignore rest))
  (with-slots ((channel %channel) (message %message)) event
    (setf channel (first (arguments event))
          message (second (arguments event)))
    (unless (char= (aref channel 0) #\#)
      (setf channel (nick event)))))

(defclass command-event (privmsg-event)
    ((%command :initarg :command :accessor command)
     (%cmd-args :initarg :cmd-args :accessor cmd-args))
    (:documentation "Event for commands."))

(define-event join-event :JOIN (channel-event) ()
    (:documentation "User join event."))

(define-event part-event :PART (channel-event) ()
    (:documentation "User part event."))

(define-event quit-event :QUIT (user-event)
    (reason)
    (:documentation "User quit event."))

(define-event mode-event :MODE (user-event)
    (target mode &optional parameter)
    (:documentation "Mode change event."))

(define-event topic-event :TOPIC (channel-event)
    (channel topic)
    (:documentation "Topic set event."))

(define-event kick-event :KICK (channel-event)
    (channel target reason)
    (:documentation "User kick event."))

(define-event notice-event :NOTICE (user-event)
    (NIL message)
    (:documentation "Notice message."))

;; REPLY CODES
(define-event welcome-event :RPL_WELCOME (event) 
    (message)
    (:documentation "Event on successful logon."))

(define-event whois-user-event :RPL_WHOISUSER (event)
    (NIL nick username hostmask NIL realname)
    (:documentation "Event on WHOISUSER response."))

(define-event whois-server-event :RPL_WHOISSERVER (event)
    (NIL nick hostname description)
    (:documentation "Event on WHOISSERVER response."))

(define-event whois-end-event :RPL_ENDOFWHOIS (event)
    (NIL nick)
    (:documentation "Event on ENDOFWHOIS response."))

(define-event whois-channels-event :RPL_WHOISCHANNELS (event)
    (NIL nick &rest channels)
    (:documentation "Event on WHOISCHANNELS response."))

(define-event whois-idle-event :RPL_WHOISIDLE (event)
    (NIL nick idle-time signon-time)
    (:documentation "Event on WHOISIDLE response."))

(define-event motd-event :RPL_MOTD (event)
    (NIL message)
    (:documentation "Event on a MOTD line."))

(define-event motd-end-event :RPL_ENDOFMOTD (event) ()
    (:documentation "Event on a ENDOFMOTD line."))


;;;;;;;;;;;;;;;; AUTOGENERATED ;;;;;;;;;;;;;;;;;;;;;;;

(defun print-generated-event-definition (name origin format comment)
  (setf name (string-trim '(#\Newline #\Linefeed #\Space) name))
  (setf origin (string-trim '(#\Newline #\Linefeed #\Space) origin))
  (setf format (string-trim '(#\Newline #\Linefeed #\Space) format))
  (setf comment (string-trim '(#\Newline #\Linefeed #\Space) comment))
  (let ((class-name (cl-ppcre:regex-replace-all "_" (string-downcase (subseq name 4)) "-"))
        (format (CL-PPCRE:REGEX-REPLACE-ALL "[\\n]" (cl-ppcre:regex-replace-all "_" (cl-ppcre:regex-replace-all "[<>:]" (string-downcase format) "") "-") " "))
        (comment (CL-PPCRE:REGEX-REPLACE-ALL "[ ]{2}" (CL-PPCRE:REGEX-REPLACE-ALL "[\\n]" comment " ") " ")))
    (format T "(define-event ~a-event :~a (event)~:[ ()~;~%    (~a)~]~%    (:documentation \"~a\"))~%~%" class-name name (> (length format) 1) format comment)))

(defun generate-event-definitions (path)
  ($ (initialize path :type :HTML) "tbody tr" 
     (each #'(lambda (a) 
               (print-generated-event-definition 
                ($ a "td" (eq 1) (text) (node)) 
                ($ a "td" (eq 2) (text) (node)) 
                ($ a "td" (eq 3) (text) (node)) 
                ($ a "td" (eq 4) (text) (node))) T)))
  NIL)
