#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.twitch
  (:nicknames #:co-twitch)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.twitch)

(define-module twitch ()
  ())

(define-group twitch :documentation "Access to twitch commands.")

(defmacro define-twitch-command (name args options &body body)
  (let ((true-args ()))
    (loop with opt = NIL
          with chan = NIL
          for arg in args
          do (when (eql arg '&optional) (setf opt T))
             (when (eql (if (listp arg) (car arg) arg) 'channel) (setf chan T))
             (push arg true-args)
          finally (unless chan
                    (unless opt (push '&optional true-args))
                    (push '(channel (channel event)) true-args)))
    `(define-command (twitch ,name) ,(nreverse true-args) (,@options :authorization T)
       (flet ((send (message &rest args)
                (irc:privmsg channel (apply #'format NIL message args) :server (server :twitch))))
         ,@body))))

(define-twitch-command mods () ()
  (send ".mods"))

(define-twitch-command color (color) ()
  (send ".color ~a" color)
  (respond event "Color changed to ~a." color))

(define-twitch-command ignore (user) ()
  (send ".ignore ~a" user)
  (respond event "User ~s ignored." user))

(define-twitch-command unignore (user) ()
  (send ".unignore ~a" user)
  (respond event "User ~s unignored." user))

(define-twitch-command me (text) ()
  (send ".me ~a" text))

(define-twitch-command timeout (user &optional time) ()
  (send ".timeout ~a~@[ ~a~]" user time)
  (respond event "User ~s has been timed out~@[ for ~a seconds~]." user time))

(define-twitch-command ban (user) ()
  (send ".ban ~a" user)
  (respond event "User ~s has been banned." user))

(define-twitch-command unban (user) ()
  (send ".unban ~a" user)
  (respond event "User ~s has been unbanned." user))

(define-twitch-command slow (&optional seconds) ()
  (cond (seconds
         (send ".slow ~a" seconds)
         (respond event "Slow mode has been activated~@[ and set to ~a seconds~]." seconds))
        (T
         (send ".slowoff")
         (respond event "Slow mode has been deactivated."))))

(define-twitch-command subscribers-only (&optional (on "on")) ()
  (cond ((string-equal on "on")
         (send ".subscribers")
         (respond event "Subscribers-only mode has been activated."))
        (T
         (send ".subscribersoff")
         (respond event "Subscribers-only mode has been deactivated."))))

(define-twitch-command emote-only (&optional (on "on")) ()
  (cond ((string-equal on "on")
         (send ".emoteonly")
         (respond event "Emote-only mode has been activated."))
        (T
         (send ".emoteoff")
         (respond event "Emote-only mode has been deactivated."))))

(define-twitch-command clear () ()
  (send ".clear")
  (respond event "Channel cleared."))

(define-twitch-command r9k (&optional (on "on")) ()
  (cond ((string-equal on "on")
         (send ".r9kbeta")
         (respond event "R9K mode has been activated."))
        (T
         (send ".r9kbetaoff")
         (respond event "R9K mode has been deactivated."))))

(define-twitch-command commercial (&optional seconds) ()
  (send ".commercial~@[ ~a~]" seconds)
  (respond event "Running a commercial~@[ for ~a seconds~]." seconds))

(define-twitch-command host (&optional channel) ()
  (cond (channel
         (send ".host ~a" channel)
         (respond event "Now hosting ~a" channel))
        (T
         (send ".unhost")
         (respond event "Now no longer hosting."))))

(define-twitch-command mod (user) ()
  (send ".mod ~a" user)
  (respond event "User ~a has been modded." user))

(define-twitch-command unmod (user) ()
  (send ".unmod ~a" user)
  (respond event "User ~a has been unmodded." user))

(defvar *emotes* ())

(defmethod start :after ((twitch twitch))
  (load-emotes))

(defun json-request (url &rest keywords)
  (let* ((drakma:*text-content-types* (list* '("application" . "json") '("text" . "json") drakma:*text-content-types*))
         (data (drakma:http-request url :external-format-in :utf-8 :external-format-out :utf-8)))
    (apply #'jsown:parse data keywords)))

(defun load-emotes ()
  ;; We load this file instead as it is more lightweight for what we want.
  (let ((raw (json-request "https://twitchemotes.com/api_cache/v2/images.json" "images")))
    (setf *emotes* (mapcar (lambda (obj) (cdr (assoc "code" (cddr obj) :test #'string=)))
                           (cddr (second raw))))))

(define-twitch-command reload-emotes () ()
  (load-emotes)
  (respond event "Emotes reloaded."))

(define-twitch-command ban-emotes (&optional (mode "600") (channel (channel event))) ()
  (let ((mode (cond ((string-equal mode "none") NIL)
                    ((string-equal mode "scold") :scold)
                    ((string-equal mode "infinity") :infinity)
                    ((parse-integer mode :junk-allowed T)
                     (parse-integer mode :junk-allowed T)))))
    (if mode
        (respond event "Emotes are now banned.")
        (respond event "Emotes are now unbanned."))))

(define-handler (privmsg-event event) ()
  (let ((timeout (gethash (channel event) (uc:config-tree :emotes-banned))))
    (when (and timeout (not (eql timeout :none)))
      (loop for emote in *emotes*
            do (when (search emote (message event) :test #'char=)
                 (case timeout
                   (:scold
                    (respond event "~a: Please do not make an embarrassment of yourself by using emotes." (nick event)))
                   (:infinity
                    (respond event "~a: You have been banned for using an emote. Please mind the rules." (nick event))
                    (respond event ".ban ~a" (nick event)))
                   (T
                    (respond event "~a: You have been timed out~@[ for ~a seconds~] for using an emote. Please mind the rules." (nick event) timeout)
                    (respond event ".timeout ~a~@[ ~a~]" timeout)))
                 (return))))))
