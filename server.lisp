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
   (%socket :initarg :socket :accessor socket)
   (%stream :initarg :stream :accessor socket-stream)

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

(defmethod connect ((server symbol) &key (host (server-config server :host))
                                      (port (server-config server :port))
                                      (nick (server-config server :nick))
                                      (user (server-config server :user))
                                      (pass (server-config server :pass))
                                      (real (server-config server :real)))
  (if (null (config-tree :servers server))
      (v:warn server "No configuration found!"))
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
      (setf (read-thread server)
            (make-thread #'(lambda ()
                             (v:info (name *current-server*) "Starting read-thread.")
                             (reconnect-loop)) 
                         :initial-bindings `((*current-server* . ,server)))))
    server))

(defmethod disconnect ((server symbol) &key (quit-message (config-tree :messages :quit)))
  (assert (not (null (gethash server *servers*))) () "Connection ~a not found!" server)
  (disconnect (gethash server *servers*) :quit-message quit-message))

(defmethod disconnect ((server server) &key (quit-message (config-tree :messages :quit)))
  (when (thread-alive-p (read-thread server))
    (v:info (name server) "Interrupting read thread...")
    (interrupt-thread (read-thread server) #'(lambda () (error 'disconnect))))

  (ignore-errors 
    (with-accessors ((socket socket)) server
      (when socket
        (v:info (name server) "Disconnecting...")
        (irc:quit :quit-message (or quit-message "#1=(quit . #1#)")))
      (setf socket NIL)))
  (remhash (name server) *servers*))

(defun reconnect (server)
  (v:warn (name server) "Connection lost, attempting reconnect in 5s...")
  (sleep 5)
  (connect server :start-thread NIL)
  (invoke-restart 'continue))

(defun reconnect-loop (&optional (server *current-server*))
  "Handles disconnection conditions and automatically attempts to reconnect."
  (handler-case 
      (handler-bind
          ((usocket:ns-try-again-condition 
            #'(lambda (err)
                (v:warn (name server) "Error encountered: ~a" err)
                (reconnect server))))
        (receive-loop server))
    (disconnect (e)
      (declare (ignore e))
      (v:warn (name server) "Leaving reconnect-loop due to disconnect condition...")))
  (v:debug (name server) "Leaving reconnect loop."))

;; Adapted from trivial-irc
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

(defvar *irc-message-regex* (cl-ppcre:create-scanner "^(:([^ ]+) +)?([^ ]+)( +(.+))?"))

(defun receive-loop (&optional (server *current-server*))
  "Continuously receives and handles a message."
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
      (v:debug name "Leaving receive loop."))))
