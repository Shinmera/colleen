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

(define-command reload essentials () (:authorization T :documentation "Reload the configuration.")
  (when (auth-p (nick event))
    (respond event (fstd-message event :config-reload "Reloading configuration."))
    (load-config)))

(define-command shutdown essentials () (:authorization T :documentation "Disconnect from all servers.")
  (when (auth-p (nick event))
    (irc:broadcast (fstd-message event :shutdown))
    (loop for server being the hash-keys of *servers*
       do (disconnect server))))

(define-command error essentials () (:documentation "Simulate a condition.")
  (error "Condition as per error function initiated by ~a in ~a." (nick event) (channel event)))

(define-command echo essentials (&rest args) (:documentation "Echo back the arguments.")
  (respond event "~{~a ~}" args))

(define-command time essentials () (:documentation "Show the current bot-local time.")
  (respond event "~a: It is now ~a" (nick event)
           (format-timestring NIL (now) :format 
                              '((:year 4) #\. :month #\. :day #\, #\Space :long-weekday #\Space :hour #\: :min #\: :sec #\Space #\( :timezone #\/ #\G #\M #\T :gmt-offset #\)))))

(define-command help essentials (command) (:documentation "Display help on a command.")
  (loop for module being the hash-values of *bot-modules*
     do (when (active module)
          (let ((method (gethash (string-downcase command) (colleen::commands module))))
            (when method
              (respond event "~a" (docu method))
              (respond event "USAGE: ~a ~{~a~^ ~}" (cmd-args method))
              (return NIL))))))

;; MODULE COMMANDS
(define-group module essentials :documentation "Manage bot modules.")

(define-command (module start) essentials (module-name) (:authorization T :documentation "Start up a module.")
  (handler-case
      (progn (start-module (find-symbol (string-upcase module-name) "KEYWORD"))
             (respond event "Module started."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (module stop) essentials (module-name) (:authorization T :documentation "Stop a module.")
  (handler-case
      (progn (stop-module (find-symbol (string-upcase module-name) "KEYWORD"))
             (respond event "Module stopped."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (module list) essentials () (:documentation "List available modules.")
  (respond event "Modules: ~{~a~^ ~}" (hash-table-keys *bot-modules*)))

(define-command (module help) essentials (module) (:documentation "Show the docstring for a module.")
  (let ((instance (get-module (find-symbol (string-upcase module) "KEYWORD"))))
    (if instance
        (respond event "~a: ~a" module (or (documentation (class-of instance) T) "No help available."))
        (respond event "No such module \"~a\"." module))))

;; IRC COMMANDS
(define-group irc essentials :documentation "Manage IRC commands.")

(defmacro define-irc-server-command (command (&rest args) (&key (authorization T) documentation) &body body)
  `(define-command (irc ,command) essentials (,@args) (:authorization ,authorization :documentation ,documentation)
     (let ((serv (if server (get-server (find-symbol server "KEYWORD")) (server event))))
       (if serv
           (progn ,@body)
           (respond event "No such server \"~a\"" server)))))

(define-command (irc raw) essentials (&rest message) (:authorization T :documentation "Send a raw line to the current IRC server.")
  (irc:send-raw (format NIL "~{~a~^ ~}" message)))

(define-irc-server-command join (channel &optional key server) (:documentation "Make the bot join a channel.")
  (irc:join channel :key key :server serv))

(define-irc-server-command part (channel &optional server) (:documentation "Make the bot part a channel.")
  (irc:part channel :server serv))

(define-irc-server-command channels (&optional server) (:authorization NIL :documentation "List the channels the bot is in.")
  (respond event "Joined channels: ~{~a~^, ~}" (channels serv)))

(define-irc-server-command quit (&optional server &rest message) (:documentation "Make the bot quit a server.")
  (disconnect serv :quit-message (format NIL "~{~a~^ ~}" message)))

(define-command (irc privmsg) essentials (target &rest message) (:authorization T :documentation "Make the bot send a message.")
  (irc:privmsg target (format NIL "~{~a~^ ~}" message)))

(define-command (irc notice) essentials (target &rest message) (:authorization T :documentation "Make the bot send a notice.")
  (irc:notice target (format NIL "~{~a~^ ~}" message)))

(define-command (irc broadcast) essentials (&rest message) (:authorization T :documentation "Broadcast a message to all channels and servers.")
  (irc:broadcast (format NIL "~{~a~^ ~}" message)))

(define-irc-server-command nick (new-nick &optional server) (:documentation "Change the bot's nick.")
  (irc:nick new-nick :server serv))

(define-command (irc topic) essentials (channel &rest topic) (:authorization T :documentation "Change the channel topic.")
  (irc:topic channel :topic (format NIL "~{~a~^ ~}" topic)))

(define-command (irc kick) essentials (nick &rest reason) (:authorization T :documentation "Kick a user from the channel.")
  (irc:kick (channel event) nick :reason reason))

(define-irc-server-command mode (target mode &optional server) (:documentation "Issue a mode change command.")
  (if (string= target "#" :end1 1)
      (irc:channel-mode target mode :server serv)
      (irc:user-mode target mode :server serv)))

