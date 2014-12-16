#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(define-module core () () (:documentation "Colleen core module, handling a few standard events."))

;; Bot internals
(defmethod start ((core core))
  (schedule-timer 'thread-sweeper :every '(:hour 1) :if-exists NIL))

(define-timer thread-sweeper () (:type :single :documentation "Regularly performs (sweep-all-module-threads)")
  (v:info :core "Performing thread sweep.")
  (sweep-all-module-threads))

(defmethod stop ((core core))
  (v:info :core "Saving colleen config.")
  (save-config))

;; Basic ping pong and nick handling.
(define-handler (events:welcome-event event) ()
  (v:info (name (server event)) "Got welcome, joining channels.")
  (let ((nickservpw (server-config (name (server event)) :nickservpw)))
    (when nickservpw
      (v:info (name (server event)) "Sending Nickserv: IDENTIFY ~a" nickservpw)
      (irc:privmsg "NickServ" (format NIL "IDENTIFY ~a" nickservpw))))
  
  (loop for chan in (server-config (name *current-server*) :channels)
     do (irc:join chan)
        (irc:privmsg chan (standard-message :join))))

(define-handler (events:pong-event event) ()
  (setf (last-ping (server event)) (get-universal-time)))

(define-handler (events:ping-event event) ()
  (setf (last-ping (server event)) (get-universal-time))
  (irc:pong (events:server1 event)))

(define-handler (events:nick-event event) ()
  (when (string-equal (events:new-nick event) (nick (server event)))
    (v:debug (name (server event)) "Changing nick of server to ~a due to NICK event." (events:new-nick event))
    (setf (nick (server event)) (events:new-nick event))))

(define-handler (events:nickname-in-use-event event) ()
  (let ((true-nick (nick (server event)))
        (nick-pass (server-config (name (server event)) :nickservpw)))
    (when (string-equal (nick event) true-nick)
      (irc:nick (format NIL "~a_" true-nick))
      (setf (nick (server event)) (format NIL "~a_" true-nick))
      (when nick-pass
        (sleep 1)
        (irc:privmsg "NickServ" (format NIL "GHOST ~a ~a" (server-config (name (server event)) :nick) nick-pass))))))

(define-handler (events:quit-event event) ()
  (let ((true-nick (server-config (name (server event)) :nick)))
    (when (and (string-equal (nick (server event)) (format NIL "~a_" true-nick))
               (string-equal (nick event) true-nick))
      (irc:nick true-nick))))

(define-handler (events:kick-event event) ()
  ;; If we get kicked, part so that the server's channel list stays proper.
  (when (string-equal (events:target event) (nick (server event)))
    (irc:part (channel event))))

;; Channel user list handling.
(defvar *channel-user-list* ())

(defun users (channel &optional (server *current-server*))
  "Retrieve a list of currently active users in CHANNEL.

Returns two values: The list of nicks and whether the channel
is even on record or not."
  (let ((server (ensure-server server)))
    (loop for (ident . users) in *channel-user-list*
          for (cur-server . cur-channel) = ident
          when (and (eql cur-server server)
                    (string-equal cur-channel channel))
          do (return (values users T))
          finally (return (values NIL NIL)))))

(defun (setf users) (users channel &optional (server *current-server*))
  "Sets the USERS list for CHANNEL."
  (let ((server (ensure-server server)))
    (loop for list in *channel-user-list*
          for (cur-server . cur-channel) = (car list) 
          when (and (eql cur-server server)
                    (string-equal cur-channel channel))
          do (return (setf (cdr list) users))
          finally (push (list* (cons server channel) users)
                        *channel-user-list*)))
  users)

;; Explicit nick list retrieval
(define-handler (events:namreply-event event) ()
  (dolist (nick (events:nickinfo event))
    ;; Cut off note
    (let ((nick (case (aref nick 0)
                  ((#\@ #\+  #\~) (subseq nick 1))
                  (T nick))))
      (pushnew nick (users (channel event) (server event))
               :test #'string-equal))))

;;; Implicit balancing
;; We want these all to run last so that modules can make comparisons of their own
;; to see where users quit from.
(define-handler (events:join-event event) (:identifier userlist-balancer-join :priority :last)
  (pushnew (nick event) (users (channel event) (server event))
           :test #'string-equal))

(define-handler (events:part-event event) (:identifier userlist-balancer-part :priority :last)
  (setf (users (channel event) (server event))
        (delete (nick event) (users (channel event) (server event))
                :test #'string-equal)))

(define-handler (events:kick-event event) (:identifier userlist-balancer-kick :priority :last)
  (setf (users (channel event) (server event))
        (delete (events:target event) (users (channel event) (server event))
                :test #'string-equal)))

(define-handler (events:quit-event event) (:identifier userlist-balancer-quit :priority :last)
  (dolist (channel (channels (server event)))
    (setf (users channel (server event))
          (delete (nick event) (users channel (server event))
                  :test #'string-equal))))

(defun refresh-channel-user-lists ()
  (loop for server being the hash-values of *servers*
        do (dolist (channel (channels server))
             (irc:names channel :server server))))
