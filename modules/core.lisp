#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(define-module core () () (:documentation "Colleen core module, handling a few standard events."))
(start (get-module :core))

(define-handler (welcome-event event) ()
  (v:info (name (server event)) "Got welcome, joining channels.")
  (let ((nickservpw (server-config (name (server event)) :nickservpw)))
    (when nickservpw
      (v:info (name (server event)) "Sending Nickserv: IDENTIFY ~a" nickservpw)
      (irc:privmsg "NickServ" (format NIL "IDENTIFY ~a" nickservpw))))
  
  (loop for chan in (server-config (name *current-server*) :channels)
     do (irc:join chan)
       (irc:privmsg chan (standard-message :join))))

(define-handler (pong-event event) ()
  (setf (last-ping (server event)) (get-universal-time)))

(define-handler (ping-event event) ()
  (setf (last-ping (server event)) (get-universal-time))
  (irc:pong (server1 event)))

(define-handler (nick-event event) ()
  (when (string-equal (old-nick event) (nick (server event)))
    (v:debug (name (server event)) "Changing nick of server to ~a due to NICK event." (nick event))
    (setf (nick server event) (nick event))))

(define-handler (nick-in-use-event event) ()
  (irc:nick (format NIL "~a_" (server-config (name (server event)) :nick))))
