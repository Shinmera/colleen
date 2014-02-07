#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(define-module core () () (:documentation "Colleen core module, handling a few standard events."))
(start (get-module :core))

(defmethod stop ((core core))
  (v:info :core "Saving colleen config.")
  (save-config))

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
  (let ((true-nick (server-config (name (server event)) :nick))
        (nick-pass (server-config (name (server event)) :nickservpw)))
    (when (string-equal (nick event) true-nick)
      (irc:nick (format NIL "~a_" true-nick))
      (setf (nick (server event)) (format NIL "~a_" true-nick))
      (when nick-pass
        (sleep 1)
        (irc:privmsg "NickServ" (format NIL "GHOST ~a ~a" true-nick nick-pass))))))

(define-handler (events:quit-event event) ()
  (let ((true-nick (server-config (name (server event)) :nick)))
    (when (and (string-equal (nick (server event)) (format NIL "~a_" true-nick))
               (string-equal (nick event) true-nick))
      (irc:nick true-nick))))
