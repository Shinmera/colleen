#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.chatlog
  (:use :cl :colleen)
  (:shadowing-import-from :colleen :restart))
(in-package :org.tymoonnext.colleen.mod.chatlog)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :clsql)
  (ql:quickload :clsql-mysql))

(define-module chatlog ()
  ((%active-in :initarg :active-in :initform () :accessor active-in)
   (%db :initarg :db :accessor db)
   (%host :initarg :host :accessor host)
   (%port :initarg :port :accessor port)
   (%user :initarg :user :accessor user)
   (%pass :initarg :pass :accessor pass))
  (:documentation "Logs messages in channels to a database."))

(defmethod start ((chatlog chatlog))
  (setf (active-in chatlog) (config-tree :chatlog :active))
  (connect-db chatlog
              :db (config-tree :chatlog :db)
              :host (config-tree :chatlog :host)
              :port (config-tree :chatlog :port)
              :user (config-tree :chatlog :user)
              :pass (config-tree :chatlog :pass)))

(defmethod stop ((chatlog chatlog))
  (disconnect-db chatlog)
  (setf (config-tree :chatlog :db) (db chatlog))
  (setf (config-tree :chatlog :host) (host chatlog))
  (setf (config-tree :chatlog :port) (port chatlog))
  (setf (config-tree :chatlog :user) (user chatlog))
  (setf (config-tree :chatlog :pass) (pass chatlog))
  (setf (config-tree :chatlog :active) (active-in chatlog)))

(defmethod connect-db ((chatlog chatlog) &key (db (db chatlog)) (host (host chatlog)) (port (port chatlog)) (user (user chatlog)) (pass (pass chatlog)))
  (setf (db chatlog) db)
  (setf (host chatlog) host)
  (setf (port chatlog) port)
  (setf (user chatlog) user)
  (setf (pass chatlog) pass)
  (clsql:connect (list host db user pass port) :database-type :mysql)
  (clsql:execute-command "SET NAMES utf8")
  (clsql:execute-command "SET CHARACTER SET utf8")
  (unless (clsql:table-exists-p 'chatlog)
    (clsql:create-table 'chatlog '((server (string 36) :not-null)
                                     (channel (string 36) :not-null)
                                     (user (string 36) :not-null)
                                     (time integer :not-null)
                                     (type (string 1) :not-null)
                                     (message text)))))

(defmethod disconnect-db ((chatlog chatlog))
  (clsql:disconnect))

(defconstant +UNIX-EPOCH-DIFFERENCE+ (encode-universal-time 0 0 0 1 1 1970 0))
(defmethod insert-record ((chatlog chatlog) server channel user type message)
  (when (find (format NIL "~a/~a" server channel) (active-in chatlog) :test #'string-equal)
    (handler-bind ((clsql-sys:sql-database-error
                     #'(lambda (err)
                         (v:severe :chatlog "SQL ERROR: ~a" err)
                         (v:info :chatlog "Reconnecting and trying again.")
                         (disconnect-db chatlog)
                         (connect-db chatlog)
                         (invoke-restart 'try-again))))
      (loop until
           (with-simple-restart (try-again "Retry inserting the record.")
             (clsql:insert-records :into 'chatlog
                                   :attributes '(server channel user time type message)
                                   :values (list (format NIL "~a" server) channel user (- (get-universal-time) +UNIX-EPOCH-DIFFERENCE+) type message))
             T)))))

(define-group chatlog :documentation "Change chatlog settings.")

(define-command (chatlog acivate) (&optional channel server) (:authorization T :documentation "Activate logging for a channel." :modulevar chatlog)
  (unless channel (setf channel (channel event)))
  (unless server (setf server (name (server event))))
  (pushnew (format NIL "~a/~a" server channel) (active-in chatlog) :test #'string-equal)
  (respond event "Activated logging for ~a/~a" server channel))

(define-command (chatlog deactivate) (&optional channel server) (:authorization T :documentation "Deactivate logging for a channel." :modulevar chatlog)
  (unless channel (setf channel (channel event)))
  (unless server (setf server (name (server event))))
  (setf (active-in chatlog)
        (delete (format NIL "~a/~a" server channel) (active-in chatlog) :test #'string-equal))
  (respond event "Deactivated logging for ~a/~a" server channel))

(define-command (chatlog reconnect) (&optional db host port user pass) (:authorization T :documentation "Restart connection to the database." :modulevar chatlog)
  (disconnect-db chatlog)
  (connect-db chatlog 
              :db (or db (db chatlog)) 
              :host (or host (host chatlog)) 
              :port (or port (port chatlog)) 
              :user (or user (user chatlog)) 
              :pass (or pass (pass chatlog))))

(define-handler (privmsg-event event) (:modulevar chatlog)
  (if (and (> (length (message event)) (length " ACTION ")) (string= (message event) "ACTION" :start1 1 :end1 7))
      (insert-record chatlog (name (server event)) (channel event) (nick event) "a" (format NIL " * ~a" (subseq (message event) 7)))
      (insert-record chatlog (name (server event)) (channel event) (nick event) "m" (message event))))

(define-handler (send-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "m" (message event)))

(define-handler (nick-event event) (:modulevar chatlog)
  (dolist (channel (channels (server event)))
    (insert-record chatlog (name (server event)) channel (old-nick event) "n" (format NIL " ** NICK ~a" (nick event)))))

(define-handler (quit-event event) (:modulevar chatlog)
  (dolist (channel (channels (server event)))
    (insert-record chatlog (name (server event)) channel (nick event) "q" (format NIL " ** QUIT ~a" (reason event)))))

(define-handler (part-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "p" " ** PART"))

(define-handler (join-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "j" " ** JOIN"))

(define-handler (kick-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "k" (format NIL " ** KICK ~a (~a)" (target event) (reason event))))

(define-handler (mode-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (target event) (nick event) "o" (format NIL " ** MODE ~a ~a" (mode event) (parameter event))))

(define-handler (topic-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "t" (format NIL " ** TOPIC ~a" (topic event))))
