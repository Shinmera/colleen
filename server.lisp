#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defclass server ()
  ((%name :initarg :name :initform (error "Name required.") :accessor name)
   (%auth-users :initarg :auth-users :initform () :accessor auth-users)
   (%channels :initform () :accessor channels)

   (%read-thread :accessor read-thread)
   (%ping-thread :accessor ping-thread)
   (%restart-thread :initform () :accessor restart-thread)
   (%socket :initarg :socket :accessor socket)
   (%stream :initarg :stream :accessor socket-stream)
   (%last-ping :initform (get-universal-time) :accessor last-ping)

   (%nick :initarg :nick :initform (error "Nick required.") :accessor nick)
   (%host :initarg :host :initform (error "Host required.") :accessor host)
   (%port :initarg :port :initform 6667 :accessor port)
   (%user :initarg :username :initform NIL :accessor username)
   (%pass :initarg :password :initform NIL :accessor password)
   (%real :initarg :realname :initform "Colleen IRC Bot" :accessor realname))
  (:documentation "Class to represent server connections."))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type T)
    (format stream "~a" (name server))))

(defun get-server (keyword)
  "Retrieve a server by its keyword name, if it is connected."
  (gethash keyword *servers*))

(defgeneric connect (server-or-name &key &allow-other-keys))
(defgeneric disconnect (server-or-name &key quit-message))
(defgeneric reconnect (server-or-name &key try-again-indefinitely))

(defun make-server-thread (server slot function)
  (setf (slot-value server slot) 
        (make-thread function :initial-bindings `((*current-server* . ,server)
                                                  (*servers* . ,*servers*)))))

(defmethod connect ((server string) &key (host NIL host-s-p) (port NIL port-s-p) (nick NIL nick-s-p) (user NIL user-s-p) (pass NIL pass-s-p) (real NIL real-s-p))
  (setf server (find-symbol (string-upcase server) "KEYWORD"))
  (unless host-s-p (setf host (server-config server :host)))
  (unless port-s-p (setf port (server-config server :port)))
  (unless nick-s-p (setf nick (server-config server :nick)))
  (unless user-s-p (setf user (server-config server :user)))
  (unless pass-s-p (setf pass (server-config server :pass)))
  (unless real-s-p (setf real (server-config server :real)))
  (connect server :host host :port port :nick nick :user user :pass pass :real real))

(defmethod connect ((server symbol) &key (host (server-config server :host))
                                      (port (server-config server :port))
                                      (nick (server-config server :nick))
                                      (user (server-config server :user))
                                      (pass (server-config server :pass))
                                      (real (server-config server :real)))
  (if (null (config-tree :servers server))
      (v:warn server "No configuration found!"))
  (assert (not (gethash server *servers*)) () "Server already connected!")
  (connect (make-instance 'server :name server 
                          :host host :port port :nick nick 
                          :username user :password pass :realname real)))

(defmethod connect ((server server) &key (start-thread T))
  "Connects with the given credentials and returns the connection object."
  (let ((*current-server* server)
        (sb-impl::*default-external-format* :UTF-8)) ; Fix for non-UTF-8 defaults.
    (with-accessors ((nick nick) (realname realname)
                     (username username) (password password) 
                     (host host) (port port) (socket socket) (socket-stream socket-stream)) server
      (v:info (name server) "Connecting ~a/~a@~a:~a/~a (~a)..." username password host port nick realname)
      
      (handler-case
          (progn 
            (setf socket (usocket:socket-connect host port))
            (setf socket-stream (usocket:socket-stream socket))
            (unless username (setf username nick))
            ;; Initiate
            (when password (irc:pass password))
            (irc:nick nick)
            (irc:user username 0 "*" realname))
        (error (err) 
          (error 'connection-failed :server server :error err))))
    
    ;; Register connection and start read-loop.
    (setf (gethash (name server) *servers*) server)
    (when start-thread
      (make-server-thread server '%read-thread #'read-loop)
      (make-server-thread server '%ping-thread #'ping-loop))
    server))

(defmethod disconnect ((server string) &key (quit-message (config-tree :messages :quit)))
  (disconnect (intern (string-upcase server) "KEYWORD") :quit-message quit-message))

(defmethod disconnect ((server symbol) &key (quit-message (config-tree :messages :quit)))
  (assert (not (null (gethash server *servers*))) () "Connection ~a not found!" server)
  (disconnect (gethash server *servers*) :quit-message quit-message))

(defmethod disconnect ((server server) &key (quit-message (config-tree :messages :quit)))
  (flet ((terminate-server-thread (slot)
           (when (and (slot-value server slot) (thread-alive-p (slot-value server slot)))
             (v:debug (name server) "Interrupting ~a" slot)
             (interrupt-thread (slot-value server slot) #'(lambda () (error 'disconnect))))))
    (terminate-server-thread '%read-thread)
    (terminate-server-thread '%ping-thread))

  (ignore-errors 
    (with-accessors ((socket socket)) server
      (when socket
        (v:info (name server) "Disconnecting...")
        (irc:quit :quit-message (or quit-message "#1=(quit . #1#)") :server server))
      (setf socket NIL)))
  (remhash (name server) *servers*))

(defmethod reconnect ((server symbol) &key try-again-indefinitely)
  (assert (not (null (gethash server *servers*))) () "Connection ~a not found!" server)
  (reconnect (gethash server *servers*)) :try-again-indefinitely try-again-indefinitely)

(defmethod reconnect ((server server) &key try-again-indefinitely)
  (v:info (name server) "Reconnecting...")
  (disconnect server)
  (loop for result = (ignore-errors (connect server)) 
     do (if result
            (return-from reconnect server)
            (if try-again-indefinitely
                (progn
                  (v:warn (name server) "Reconnection failed, trying again in ~as..." (server-config server :reconnect-cooldown))
                  (sleep (server-config server :reconnect-cooldown)))
                (error 'connection-failed :server server)))))

(defmacro with-reconnect-handler (servervar &body body)
  `(handler-case
       (progn ,@body)
     ((or usocket:ns-try-again-condition 
       usocket:timeout-error 
       usocket:shutdown-error
       usocket:connection-reset-error
       usocket:connection-aborted-error
       ping-timeout
       cl:end-of-file) (err)
       (v:warn (name ,servervar) "Error encountered: ~a" err)
      (when (not (restart-thread ,servervar))
        (v:warn (name ,servervar) "Connection lost, attempting reconnect in ~as..." (server-config ,servervar :reconnect-cooldown))
        (make-server-thread ,servervar '%restart-thread
                            #'(lambda () 
                                (sleep (server-config ,servervar :reconnect-cooldown))
                                (reconnect ,servervar :try-again-indefinitely T)
                                (setf (restart-thread ,servervar) NIL)))))
     (disconnect (e)
       (declare (ignore e))
       (v:warn (name ,servervar) "Leaving reconnect-handler due to disconnect condition..."))
     (error (e)
       (v:severe (name ,servervar) "Uncaught error in reconnect-handler: ~a" e))))

(defvar *irc-message-regex* (cl-ppcre:create-scanner "^(:([^ ]+) +)?([^ ]+)( +(.+))?"))
(defun read-loop (&optional (server *current-server*))
  "Continuously receives and handles a message."
  (with-reconnect-handler server
    (flet ((prepare-arguments (arguments)
             (let ((final-arg (search " :" arguments)))
               (append (split-sequence #\Space arguments :remove-empty-subseqs t :end final-arg)
                       (when final-arg (list (subseq arguments (+ 2 final-arg))))))))
      (let ((name (name server)))
        (with-simple-restart (exit "<~a> Exit the receive loop." name)
          (loop (with-simple-restart (continue "<~a> Continue reading messages." name)
                  (v:trace name "Reading message...")
                  (let ((message (receive-raw-message server)))
                    (v:trace name "Read message: ~a" message)
                    (cl-ppcre:register-groups-bind (NIL prefix command arguments NIL) (*irc-message-regex* message)
                      (setf arguments (prepare-arguments arguments))
                      (setf command (reply->keyword command))
                      (handle command prefix arguments))))))
        (v:debug name "Leaving read loop.")))))

(defun receive-raw-message (&optional (server *current-server*))
  (handler-bind ((sb-int:stream-decoding-error
                  #'(lambda (err)
                      (v:warn (name server) "Stream decoding error: ~a" err)
                      (invoke-restart 'sb-int:attempt-resync))))
    (with-output-to-string (message)
      (with-accessors ((stream socket-stream)) server
        (loop for char = (read-char stream)
           until (and (eql #\Return char)
                      (eql #\Linefeed (peek-char nil stream)))
           do (write-char char message)
           finally (read-char stream))))))

(defun ping-loop (&optional (server *current-server*))
  (with-reconnect-handler server
    (let ((name (name server)))
      (with-simple-restart (exit "<~a> Exit the ping loop." name)
        (loop (with-simple-restart (continue "<~a> Continue pinging." name)
                (let ((diff (- (get-universal-time) (last-ping server))))
                  (v:trace name "Last ping T-~a" diff)
                  (when (and (> diff (- (server-config (name server) :ping-warn) (/ (server-config (name server) :ping-step) 2))) 
                             (< diff (+ (server-config (name server) :ping-warn) (/ (server-config (name server) :ping-step) 2))))
                    (v:warn name "No ping reply in ~as!" diff))
                  (when (> diff (server-config (name server) :ping-timeout))
                    (v:warn name "Ping timeout!")
                    (error 'ping-timeout :server server)))
                (v:trace name "Sending ping...")
                (irc:ping (host server))
                (sleep (server-config (name server) :ping-step)))))
      (v:debug name "Leaving ping loop."))))
