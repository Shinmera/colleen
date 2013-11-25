#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.essentials
  (:use :cl :colleen :local-time :alexandria)
  (:shadowing-import-from :colleen :restart)
  (:shadow :shutdown))
(in-package :org.tymoonnext.colleen.mod.essentials)

(define-module essentials ()
  ())

(define-command reload essentials () (:authorization T)
  (when (auth-p (nick event))
    (respond event (fstd-message event :config-reload "Reloading configuration."))
    (load-config)))

(define-command shutdown essentials () (:authorization T)
  (when (auth-p (nick event))
    (irc:broadcast (fstd-message event :shutdown))
    (loop for server being the hash-keys of *servers*
       do (disconnect server))))

(define-command (make-error error) essentials () ()
  (error "Condition as per error function initiated by ~a in ~a." (nick event) (channel event)))

(define-command echo essentials (&rest args) ()
  (respond event "~{~a ~}" args))

(define-command (send-time time) essentials () ()
  (respond event "~a: It is now ~a" (nick event)
           (format-timestring NIL (now) :format 
                              '((:year 4) #\. :month #\. :day #\, #\Space :long-weekday #\Space :hour #\: :min #\: :sec #\Space #\( :timezone #\/ #\G #\M #\T :gmt-offset #\)))))

;; MODULE COMMANDS
(define-group module essentials (:documentation "Manage bot modules."))

(define-command (%start start) essentials (module-name) (:group 'module :authorization T :documentation "Start up a module.")
  (handler-case
      (progn (start-module (find-symbol (string-upcase module-name) "KEYWORD"))
             (respond event "Module started."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (%stop stop) essentials (module-name) (:group 'module :authorization T :documentation "Stop a module.")
  (handler-case
      (progn (stop-module (find-symbol (string-upcase module-name) "KEYWORD"))
             (respond event "Module stopped."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (%list list) essentials () (:group 'module :authorization T :documentation "List available modules.")
  (respond event "Modules: ~{~a~^ ~}" (hash-table-keys *bot-modules*)))

;; IRC COMMANDS
(define-group irc essentials (:documentation "Manage IRC commands."))

(defmacro define-irc-server-command (command (&rest args) (&key (authorization T) documentation) &body body)
  `(define-command (,(intern (format NIL "%~a" command)) ,command) essentials (,@args) (:group 'irc :authorization ,authorization :documentation ,documentation)
     (let ((serv (if server (get-server (find-symbol server "KEYWORD")) (server event))))
       (if serv
           (progn ,@body)
           (respond event "No such server \"~a\"" server)))))

(define-command (%raw raw) essentials (&rest message) (:group 'irc :authorization T :documentation "Send a raw line to the current IRC server.")
  (irc:send-raw (format NIL "~{~a~^ ~}" message)))

(define-irc-server-command join (channel &optional key server) (:documentation "Make the bot join a channel.")
  (irc:join channel :key key :server serv))

(define-irc-server-command part (channel &optional server) (:documentation "Make the bot part a channel.")
  (irc:part channel :server serv))

(define-irc-server-command channels (&optional server) (:authorization NIL :documentation "List the channels the bot is in.")
  (respond event "Joined channels: ~{~a~^, ~}" (channels serv)))

(define-irc-server-command quit (&optional server &rest message) (:documentation "Make the bot quit a server.")
  (disconnect serv :quit-message (format NIL "~{~a~^ ~}" message)))

(define-command (%privmsg privmsg) essentials (target &rest message) (:group 'irc :authorization T :documentation "Make the bot send a message.")
  (irc:privmsg target (format NIL "~{~a~^ ~}" message)))

(define-command (%notice notice) essentials (target &rest message) (:group 'irc :authorization T :documentation "Make the bot send a notice.")
  (irc:notice target (format NIL "~{~a~^ ~}" message)))

(define-command (%broadcast broadcast) essentials (&rest message) (:group 'irc :authorization T :documentation "Broadcast a message to all channels and servers.")
  (irc:broadcast (format NIL "~{~a~^ ~}" message)))

(define-irc-server-command nick (new-nick &optional server) (:documentation "Change the bot's nick.")
  (irc:nick new-nick :server serv))

(define-command (%topic topic) essentials (channel &rest topic) (:group 'irc :authorization T :documentation "Change the channel topic.")
  (irc:topic channel :topic (format NIL "~{~a~^ ~}" topic)))

(define-command (%kick kick) essentials (nick &rest reason) (:group 'irc :authorization T :documentation "Kick a user from the channel.")
  (irc:kick (channel event) nick :reason reason))

(define-irc-server-command mode (target mode &optional server) (:documentation "Issue a mode change command.")
  (if (string= target "#" :end1 1)
      (irc:channel-mode target mode :server serv)
      (irc:user-mode target mode :server serv)))

