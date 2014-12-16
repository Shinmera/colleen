#|
  This file is a part of Colleen
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.colleen)
(defpackage #:org.tymoonnext.colleen.mod.chatlog
  (:nicknames #:co-chatlog)
  (:use #:cl #:colleen #:events))
(in-package #:org.tymoonnext.colleen.mod.chatlog)

(define-module chatlog ()
  ((%lock :initform (bordeaux-threads:make-lock "DATABASE") :accessor lock))
  (:documentation "Logs messages in channels to a database."))

(defmethod connect-db ((chatlog chatlog) &key db host port user pass)
  (with-module-storage (chatlog)
    (clsql:connect (list (or host (uc:config-tree :host))
                         (or db (uc:config-tree :db))
                         (or user (uc:config-tree :user))
                         (or pass (uc:config-tree :pass))
                         (or port (uc:config-tree :port))) :database-type :mysql)
    (clsql:execute-command "SET NAMES utf8")
    (clsql:execute-command "SET CHARACTER SET utf8")
    (unless (clsql:table-exists-p 'chatlog)
      (clsql:create-table 'chatlog '((server (string 36) :not-null)
                                     (channel (string 36) :not-null)
                                     (user (string 36) :not-null)
                                     (time integer :not-null)
                                     (type (string 1) :not-null)
                                     (message text))))))

(defmethod disconnect-db ((chatlog chatlog))
  (clsql:disconnect))

(defconstant +UNIX-EPOCH-DIFFERENCE+ (encode-universal-time 0 0 0 1 1 1970 0))
(defmethod insert-record ((chatlog chatlog) server channel user type message)
  (with-module-storage (chatlog)
    (when (find (format NIL "~a/~a" server channel) (uc:config-tree :active-in) :test #'string-equal)
      (unless (clsql:connected-databases) (connect-db chatlog))
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
                T))))))

(define-group chatlog :documentation "Change chatlog settings.")

(define-command (chatlog activate) (&optional channel server) (:authorization T :documentation "Activate logging for a channel." :modulevar chatlog)
  (unless channel (setf channel (channel event)))
  (unless server (setf server (name (server event))))
  (pushnew (format NIL "~a/~a" server channel) (uc:config-tree :active-in) :test #'string-equal)
  (respond event "Activated logging for ~a/~a" server channel))

(define-command (chatlog deactivate) (&optional channel server) (:authorization T :documentation "Deactivate logging for a channel." :modulevar chatlog)
  (unless channel (setf channel (channel event)))
  (unless server (setf server (name (server event))))
  (setf (uc:config-tree :active-in)
        (delete (format NIL "~a/~a" server channel) (uc:config-tree :active-in) :test #'string-equal))
  (respond event "Deactivated logging for ~a/~a" server channel))

(define-command (chatlog reconnect) (&optional db host port user pass) (:authorization T :documentation "Restart connection to the database." :modulevar chatlog)
  (disconnect-db chatlog)
  (connect-db chatlog :db db :host host :port port :user user :pass pass)
  (disconnect-db chatlog))

(define-handler (privmsg-event event) (:modulevar chatlog)
  (if (and (> (length (message event)) (length " ACTION ")) (string= (message event) "ACTION" :start1 1 :end1 7))
      (insert-record chatlog (name (server event)) (channel event) (nick event) "a" (format NIL " * ~a" (subseq (message event) 7)))
      (insert-record chatlog (name (server event)) (channel event) (nick event) "m" (message event))))

(define-handler (send-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "m" (message event)))

(define-handler (nick-event event) (:modulevar chatlog)
  (dolist (channel (channels (server event)))
    (insert-record chatlog (name (server event)) channel (nick event) "n" (format NIL " ** NICK ~a" (new-nick event)))))

(define-handler (quit-event event) (:modulevar chatlog)
  (dolist (channel (channels (server event)))
    (when (find (nick event) (users channel (server event)) :test #'string-equal)
      (insert-record chatlog (name (server event)) channel (nick event) "q" (format NIL " ** QUIT ~a" (reason event))))))

(define-handler (part-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "p" " ** PART"))

(define-handler (join-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "j" " ** JOIN"))

(define-handler (kick-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "k" (format NIL " ** KICK ~a (~a)" (target event) (reason event))))

(define-handler (mode-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (target event) (nick event) "o" (format NIL " ** MODE ~a ~a" (mode event) (parameter event))))

(define-handler (topic-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (channel event) "t" (format NIL " ** TOPIC ~a" (topic event))))

(define-handler (topic-set-event event) (:modulevar chatlog)
  (insert-record chatlog (name (server event)) (channel event) (nick event) "t" (format NIL " ** TOPIC ~a" (topic event))))

(define-command (chatlog stats) (&optional channel server) (:documentation "Retrieve short statistics about the recorded chatlog history.")
  (flet ((channel-char-p (c) (find (char-downcase c) "abcdefghijklmnopqrstuvwxyz-#_")))
    (setf channel (if channel
                      (remove-if-not #'channel-char-p channel)
                      (channel event)))
    (setf server (if server
                     (remove-if-not #'channel-char-p server)
                     (name (server event))))
    (unless (clsql:connected-databases) (connect-db module))
    (let ((most-active-user (first (clsql:select 'user (clsql:sql-operation 'count '*)
                                                 :from 'chatlog
                                                 :where (clsql:sql-operation 'and (clsql:sql-operation '= 'channel channel)
                                                                             (clsql:sql-operation '= 'server server))
                                                 :group-by 'user :order-by `((,(clsql:sql-operation 'count '*) desc)) :limit 1)))
          (total-messages (first (clsql:select (clsql:sql-operation 'count '*)
                                               :from 'chatlog
                                               :where (clsql:sql-operation 'and (clsql:sql-operation '= 'channel channel)
                                                                           (clsql:sql-operation '= 'server server)))))
          (earliest-time (first (clsql:select 'time
                                              :from 'chatlog
                                              :where (clsql:sql-operation 'and (clsql:sql-operation '= 'channel channel)
                                                                          (clsql:sql-operation '= 'server server))
                                              :order-by '((time asc)) :limit 1))))
      (respond event "Log stats for ~a/~a: Most active user is ~a with ~,,'':d entries. A total of ~,,'':d entries have been recorded for this channel since ~a."
               server channel (first most-active-user) (second most-active-user) (first total-messages)
               (local-time:format-timestring NIL (local-time:unix-to-timestamp (first earliest-time)) :format '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))))))

(define-command (chatlog count) (nick) (:documentation "Counts the amount of recorded messages for a nick.")
  (unless (clsql:connected-databases) (connect-db module))
  (let ((messages (first (clsql:select (clsql:sql-operation 'count '*)
                                       :from 'chatlog
                                       :where (clsql:sql-operation '= 'user nick)
                                       :limit 1))))
    (respond event "Recorded messages for ~a: ~,,'':d" nick (first messages))))
