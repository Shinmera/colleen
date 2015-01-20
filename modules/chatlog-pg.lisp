#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.chatlog-pg
  (:nicknames #:co-chatlog-pg)
  (:use #:cl #:colleen #:events)
  (:export
   #:active-p
   #:external-address))
(in-package #:org.shirakumo.colleen.mod.chatlog-pg)

(defconstant +UNIX-EPOCH-DIFFERENCE+ (encode-universal-time 0 0 0 1 1 1970 0))
(defvar *connection-active* NIL)

(defun get-unix-time ()
  (- (get-universal-time) +UNIX-EPOCH-DIFFERENCE+))

(define-module chatlog-pg ()
  ()
  (:documentation "Logs messages in channels to a database."))

(defun ident (server channel)
  (format NIL "~a/~a" server channel))

(defun active-p (server channel)
  (with-module-storage ((module :chatlog-pg))
    (find (ident server channel) (uc:config-tree :active-in)
          :test #'string-equal)))

(defun (setf active-p) (val server channel)
  (with-module-storage ((module :chatlog-pg))
    (if val
        (pushnew (ident server channel) (uc:config-tree :active-in)
                 :test #'string-equal)
        (setf (uc:config-tree :active-in)
              (delete (ident server channel) (uc:config-tree :active-in)
                      :test #'string-equal)))))

(defun connection ()
  (with-module-storage ((module :chatlog-pg))
    (list (uc:config-tree :user)
          (uc:config-tree :pass)
          (uc:config-tree :db)
          (uc:config-tree :host)
          (uc:config-tree :port))))

(defun (setf connection) (list)
  (destructuring-bind (user pass &optional (db "irc") (host "localhost") (port "5432")) list
    (with-module-storage ((module :chatlog-pg))
      (setf (uc:config-tree :user) user
            (uc:config-tree :pass) pass
            (uc:config-tree :db) db
            (uc:config-tree :host) host
            (uc:config-tree :port) port)))
  list)

(defun external-address (&optional server channel time)
  (with-module-storage ((module :chatlog-pg))
    (when (uc:config-tree :external-address)
      (format NIL "~a~@[~(~a~)/~]~@[~(~a~)~]~@[?around=~d#~:*~d~]"
              (uc:config-tree :external-address) server (string-trim "# " channel) time))))

(defun (setf external-address) (address)
  (with-module-storage ((module :chatlog-pg))
    (setf (uc:config-tree :external-address) address)))

(defmethod start ((chatlog chatlog-pg))
  (when (apply #'test-connection (connection))
    (setf *connection-active* (connection))))

(defun test-connection (user pass &optional (db "irc") (host "localhost") (port 5432))
  (v:info :chatlog-pg "Testing ~a/~v@{*~}@~a:~a/~a" user (length pass) host port db)
  (handler-case
      (postmodern:with-connection (list db user pass host :port port)
        (unless (postmodern:table-exists-p "chatlog")
          (postmodern:execute "CREATE TABLE \"chatlog\"
 (\"server\" character varying(36) NOT NULL,
  \"channel\" character varying(36) NOT NULL,
  \"nick\" character varying(36) NOT NULL,
  \"time\" bigint NOT NULL,
  \"type\" character(1) NOT NULL,
  \"message\" text NOT NULL)"))
        (list user pass db host port))
    (error (err)
      (v:error :chatlog-pg "Failed to test connection! ~a" err)
      NIL)))

(defun prepared-statement (statement &rest variables)
  (cl-postgres:prepare-query postmodern:*database* "" statement)
  (cl-postgres:exec-prepared postmodern:*database* "" variables
                             (cl-postgres:row-reader (fields)
                               (loop while (cl-postgres:next-row)
                                     collect (loop for field across fields
                                                   collect (cl-postgres:next-field field))))))

(defmacro with-handled-connection ((&optional (spec '(connection))) &body body)
  (let ((info (gensym "CONNECTION-INFO"))
        (user (gensym "USER"))
        (pass (gensym "PASS"))
        (db (gensym "DB"))
        (host (gensym "HOST"))
        (port (gensym "PORT")))
    `(let ((,info ,spec))
       (when (and *connection-active* ,info)
         (destructuring-bind (,user ,pass ,db ,host ,port) ,info
           (handler-case
               (postmodern:with-connection (list ,db ,user ,pass ,host :port ,port)
                 ,@body)
             (error (err)
               (v:error :chatlog-pg "Error in safe-connection block: ~a" err)
               NIL)))))))

(defun insert-record (server channel user type message)
  (let ((server (string-downcase server))
        (channel (string-downcase channel)))
    (when (active-p server channel)
      (v:debug :chatlog-pg "Logging event from ~a/~a <~a> (~a) ~s" server channel user type message)
      (with-handled-connection ()
        (prepared-statement "INSERT INTO \"chatlog\" (\"server\", \"channel\", \"nick\", \"time\", \"type\", \"message\") VALUES ($1,$2,$3,$4,$5,$6)"
                            server channel user (get-unix-time) type message)))))

(define-group chatlog-pg :documentation "Change chatlog-pg settings.")

(define-command (chatlog-pg server) (&optional user pass (db "irc") (host "localhost") (port "5432")) (:authorization T :documentation "View or switch the logging server.")
  (cond ((and user pass)
         (setf port (parse-integer port))
         (let ((con (test-connection user pass db host port)))
           (cond (con
                  (setf (connection) user pass db host port
                        *connection-active* T)))))
        (T (destructuring-bind (user pass db host port) (connection)
             (declare (ignore pass))
             (respond event "Currently connected to ~a@~a:~a/~a" user db host port)))))

(defmacro with-chan/serv ((&optional (channel 'channel) (server 'server) (event 'event)) &body body)
  `(let ((,channel (string-downcase (or ,channel (channel ,event))))
         (,server (string-downcase (or ,server (name (server ,event))))))
     ,@body))

(define-command (chatlog-pg activate) (&optional channel server) (:authorization T :documentation "Activate logging for a channel.")
  (with-chan/serv ()
    (setf (active-p channel server) T)
    (respond event "Activated logging for ~a/~a" server channel)))

(define-command (chatlog-pg deactivate) (&optional channel server) (:authorization T :documentation "Deactivate logging for a channel.")
  (with-chan/serv ()
    (setf (active-p channel server) NIL)
    (respond event "Deactivated logging for ~a/~a" server channel)))

(define-handler (privmsg-event event) ()
  (if (and (> (length (message event)) (length " ACTION ")) (string= (message event) "ACTION" :start1 1 :end1 7))
      (insert-record (name (server event)) (channel event) (nick event) "a" (format NIL " * ~a" (subseq (message event) 7)))
      (insert-record (name (server event)) (channel event) (nick event) "m" (message event))))

(define-handler (send-event event) ()
  (insert-record (name (server event)) (channel event) (nick event) "m" (message event)))

(define-handler (nick-event event) ()
  (dolist (channel (channels (server event)))
    (insert-record (name (server event)) channel (nick event) "n" (format NIL " ** NICK ~a" (new-nick event)))))

(define-handler (quit-event event) ()
  (dolist (channel (channels (server event)))
    (when (find (nick event) (users channel (server event)) :test #'string-equal)
      (insert-record (name (server event)) channel (nick event) "q" (format NIL " ** QUIT ~a" (reason event))))))

(define-handler (part-event event) ()
  (insert-record (name (server event)) (channel event) (nick event) "p" " ** PART"))

(define-handler (join-event event) ()
  (insert-record (name (server event)) (channel event) (nick event) "j" " ** JOIN"))

(define-handler (kick-event event) ()
  (insert-record (name (server event)) (channel event) (nick event) "k" (format NIL " ** KICK ~a (~a)" (target event) (reason event))))

(define-handler (mode-event event) ()
  (insert-record (name (server event)) (target event) (nick event) "o" (format NIL " ** MODE ~a ~a" (mode event) (parameter event))))

(define-handler (topic-event event) ()
  (insert-record (name (server event)) (channel event) (channel event) "t" (format NIL " ** TOPIC ~a" (topic event))))

(define-handler (topic-set-event event) ()
  (insert-record (name (server event)) (channel event) (nick event) "t" (format NIL " ** TOPIC ~a" (topic event))))

(defun %single-where (stream arg &rest rest)
  (declare (ignore rest))
  (format stream "\"~(~a~)\"=~a" (first arg) (etypecase (second arg)
                                               (string (format NIL "'~a'" (second arg)))
                                               (fixnum (format NIL "$~a" (second arg))))))


(defun format-unix-time (time)
  (if time
      (local-time:format-timestring
       NIL (local-time:unix-to-timestamp time)
       :format '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))
      "never"))

(define-command (chatlog-pg stats) (&optional channel server) (:documentation "View logging statistics about a channel.")
  (with-chan/serv ()
    (unless
        (with-handled-connection ()
          (flet ((single (field &optional where clauses &rest vals)
                   (first (apply #'prepared-statement (format NIL "SELECT ~a FROM \"chatlog\" WHERE \"channel\"=$1 AND \"server\"=$2~@[ AND ~{~/co-chatlog-pg::%single-where/~^ AND ~}~]~@[ ~a~];"
                                                              field where clauses)
                                 channel server vals))))
            (let ((events   (first (single "COUNT(*)")))
                  (messages (first (single "COUNT(*)" '(("type" "m")))))
                  (users    (first (first (prepared-statement "SELECT COUNT(*) FROM (SELECT DISTINCT \"nick\" FROM \"chatlog\" WHERE \"channel\"=$1 AND \"server\"=$2 AND \"type\"='m') AS temp;"
                                                              channel server))))
                  (since    (first (single "\"time\"" () "ORDER BY \"time\" ASC LIMIT 1")))
                  (topuser         (single "\"nick\", COUNT(*) AS c" '(("type" "m")) "GROUP BY \"nick\" ORDER BY c DESC LIMIT 1")))
              (respond event "Logging ~a since ~a with a total of ~,,'':d events, of which ~,,'':d were messages from ~,,'':d users. The user with the most messages (~,,'':d) is ~a."
                       (ident server channel) (format-unix-time since) (or events 0) (or messages 0) (or users 0) (or (second topuser) 0) (or (first topuser) "nobody"))))
          T)
      (respond event "Failed to retrieve statistics!"))))

(define-command (chatlog-pg about) (nick) (:documentation "View logging statistics about a nick.")
  (unless
      (with-handled-connection ()
        (flet ((single (field &optional where clauses &rest vals)
                 (first (apply #'prepared-statement (format NIL "SELECT ~a FROM \"chatlog\" WHERE \"nick\"=$1~@[ AND ~{~/co-chatlog-pg::%single-where/~^ AND ~}~]~@[ ~a~];"
                                                            field where clauses)
                               nick vals))))
          (let ((events   (first (single "COUNT(*)")))
                (messages (first (single "COUNT(*)" '(("type" "m")))))
                (since    (first (single "\"time\"" () "ORDER BY \"time\" ASC LIMIT 1"))))
            (respond event "Logging ~a since ~a with a total of ~,,'':d events, of which ~,,'':d were messages."
                     nick (format-unix-time since) (or events 0) (or messages 0)))))
    (respond event "Failed to retrieve statistics!")))
