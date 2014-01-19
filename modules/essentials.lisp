#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.essentials
  (:use :cl :colleen :events :local-time :alexandria)
  (:shadowing-import-from :colleen :restart)
  (:shadow :shutdown))
(in-package :org.tymoonnext.colleen.mod.essentials)

(define-module essentials ()
    ((%last-seen :initform (make-hash-table :test 'equalp) :accessor last-seen)
     (%startup :initform (get-universal-time) :accessor startup-time))
  (:documentation "A few essential bot and irc commands."))

(define-command reload () (:authorization T :documentation "Reload the configuration.")
  (when (auth-p (nick event))
    (respond event (fstd-message event :config-reload "Reloading configuration."))
    (load-config)))

(define-command connect (server) (:authorization T :documentation "Connect to a server configuration.")
  (let ((serverkey (find-symbol (string-upcase server) :KEYWORD)))
    (if serverkey
        (progn
          (connect serverkey)
          (respond event "Connected to ~a." serverkey))
        (respond event "No such server found."))))

(define-command shutdown () (:authorization T :documentation "Disconnect from all servers.")
  (when (auth-p (nick event))
    (irc:broadcast (fstd-message event :shutdown))
    (loop for server being the hash-keys of *servers*
       do (disconnect server))))

(define-command quickload (&optional (system "colleen")) (:authorization T :documentation "Perform a ql:quickload.")
  (setf system (find-symbol (string-upcase system) :KEYWORD))
  (ql:quickload system)
  (respond event "System loaded."))

(define-command error (&optional type) (:documentation "Simulate a condition.")
  (if type
      (error (find-symbol (string-upcase type)))
      (error "Condition as per error function initiated by ~a in ~a." (nick event) (channel event))))

(define-command echo (&rest args) (:documentation "Echo back the arguments.")
  (respond event "~{~a ~}" args))

(define-command time () (:documentation "Show the current bot-local time.")
  (respond event "~a: It is now ~a" (nick event)
           (format-timestring NIL (now) :format 
                              '((:year 4) #\. :month #\. :day #\, #\Space :long-weekday #\Space (:hour 2) #\: (:min 2) #\: (:sec 2) #\Space #\( :timezone #\/ #\G #\M #\T :gmt-offset #\)))))

(define-command help (&optional command) (:documentation "Display help on a command.")
  (if command
      (loop for module being the hash-values of *bot-modules*
         do (when (active module)
              (let ((method (gethash (string-downcase command) (colleen:commands module))))
                (when method
                  (respond event "~a" (docu method))
                  (respond event "USAGE: ~a ~{~a~^ ~}" (name method) (cmd-args method))
                  (return NIL)))))
      (loop with commands = () 
         for module being the hash-values of *bot-modules*
         if (active module)
         do (appendf commands (alexandria:hash-table-keys (colleen:commands module)))
         finally (respond event "Available commands: ~{~a~^, ~}" commands))))

;; MODULE COMMANDS
(define-group module :documentation "Manage bot modules.")

(define-command (module start) (module-name) (:authorization T :documentation "Start up a module.")
  (handler-case
      (progn (start-module (find-symbol (string-upcase module-name) "KEYWORD"))
             (respond event "Module started."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (module stop) (module-name) (:authorization T :documentation "Stop a module.")
  (handler-case
      (progn (stop-module (find-symbol (string-upcase module-name) "KEYWORD"))
             (respond event "Module stopped."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (module restart) (module-name) (:authorization T :documentation "Stop&Start a module.")
  (handler-case
      (progn (stop-module (find-symbol (string-upcase module-name) "KEYWORD"))
             (start-module (find-symbol (string-upcase module-name) "KEYWORD"))
             (respond event "Module restarted."))
    (error (err)
      (respond event "Error: ~a" err))))

(defun format-module (stream module-name &rest rest)
  (declare (ignore rest))
  (format stream "~:[~a~;*~a~]" (active (get-module module-name)) module-name))

(define-command (module list) () (:documentation "List available modules.")
  (respond event "Modules [* activated]: ~{~/org.tymoonnext.colleen.mod.essentials::format-module/~^ ~}" (hash-table-keys *bot-modules*)))

(defmacro with-module ((module-name modulevar) &body body)
  `(let ((,modulevar (get-module ,module-name)))
     (if ,modulevar
        (progn ,@body)
        (respond event "No such module \"~a\"." ,module-name))))

(define-command (module help) (module-name) (:documentation "Show the docstring for a module.")
  (with-module (module-name instance)
    (respond event "~a: ~a" module-name (or (documentation (class-of instance) T) "No help available."))))

(define-command (module commands) (module-name) (:documentation "List the commands a module provides.")
  (with-module (module-name instance)
    (let ((commands (alexandria:hash-table-keys (colleen:commands instance))))
      (if commands
          (respond event "~a provides: ~{~a~^, ~}" module-name commands)
          (respond event "~a does not provide any commands." module-name)))))

(define-command (module handlers) (module-name) (:documentation "List all the events this module handles.")
  (with-module (module-name instance)
    (let ((handlers (alexandria:hash-table-keys (colleen:handlers instance))))
      (if handlers
          (respond event "~a handles: ~{~a~^, ~}" module-name handlers)
          (respond event "~a does not provide any commands." module-name)))))

(define-command (module threads) (module-name) (:documentation "List threads that are currently active in this module.")
  (with-module (module-name instance)
    (let ((threads (alexandria:hash-table-keys (colleen:threads instance))))
      (if threads
          (respond event "~a runs the following threads: ~{~a~^, ~}" module-name
                   (mapcar #'(lambda (a) (format NIL "~:[~a~;*~a~]" (bordeaux-threads:thread-alive-p (gethash a (colleen:threads instance))) a)) threads))
          (respond event "~a does not have any running threads." module-name)))))

(define-command (module interrupt) (module-name) (:documentation "Interrupt all the module threads.")
  (with-module (module-name instance)
    (let ((threads (alexandria:hash-table-keys (colleen:threads instance))))
      (if threads
          (loop for uid in threads
             for thread = (gethash uid (colleen:threads instance))
             do (if (bordeaux-threads:thread-alive-p thread)
                    (bordeaux-threads:interrupt-thread thread #'(lambda () (error 'module-stop)))
                    (remhash uid (threads module)))
               (respond event "Thread ~a interrupted." uid))
          (respond event "~a does not have any running threads." module-name)))))

(define-command (module kill) (module-name) (:documentation "Kill all the module threads.")
  (with-module (module-name instance)
    (let ((threads (alexandria:hash-table-keys (colleen:threads instance))))
      (if threads
          (loop for uid in threads
             for thread = (gethash uid (colleen:threads instance))
             do (when (bordeaux-threads:thread-alive-p thread)
                  (bordeaux-threads:destroy-thread thread))
               (remhash uid (threads module))
               (respond event "Thread ~a killed." uid))
          (respond event "~a does not have any running threads." module-name)))))

;; IRC COMMANDS
(define-group irc :documentation "Manage IRC commands.")

(defmacro define-irc-server-command (command (&rest args) (&key (authorization T) documentation) &body body)
  `(define-command (irc ,command) (,@args) (:authorization ,authorization :documentation ,documentation)
     (let ((serv (if server (get-server (find-symbol server "KEYWORD")) (server event))))
       (if serv
           (progn ,@body)
           (respond event "No such server \"~a\"" server)))))

(define-command (irc raw) (&rest message) (:authorization T :documentation "Send a raw line to the current IRC server.")
  (irc:send-raw (format NIL "~{~a~^ ~}" message)))

(define-irc-server-command join (channel &optional key server) (:documentation "Make the bot join a channel.")
  (irc:join channel :key key :server serv))

(define-irc-server-command part (channel &optional server) (:documentation "Make the bot part a channel.")
  (irc:part channel :server serv))

(define-irc-server-command channels (&optional server) (:authorization NIL :documentation "List the channels the bot is in.")
  (respond event "Joined channels: ~{~a~^, ~}" (channels serv)))

(define-irc-server-command quit (&optional server &rest message) (:documentation "Make the bot quit a server.")
  (disconnect serv :quit-message (format NIL "~{~a~^ ~}" message)))

(define-command (irc privmsg) (target &rest message) (:authorization T :documentation "Make the bot send a message.")
  (irc:privmsg target (format NIL "~{~a~^ ~}" message)))

(define-command (irc notice) (target &rest message) (:authorization T :documentation "Make the bot send a notice.")
  (irc:notice target (format NIL "~{~a~^ ~}" message)))

(define-command (irc broadcast) (&rest message) (:authorization T :documentation "Broadcast a message to all channels and servers.")
  (irc:broadcast (format NIL "~{~a~^ ~}" message)))

(define-irc-server-command nick (new-nick &optional server) (:documentation "Change the bot's nick.")
  (irc:nick new-nick :server serv))

(define-command (irc topic) (channel &rest topic) (:authorization T :documentation "Change the channel topic.")
  (irc:topic channel :topic (format NIL "~{~a~^ ~}" topic)))

(define-command (irc kick) (nick &rest reason) (:authorization T :documentation "Kick a user from the channel.")
  (irc:kick (channel event) nick :reason reason))

(define-irc-server-command mode (target mode &optional server) (:documentation "Issue a mode change command.")
  (if (string= target "#" :end1 1)
      (irc:channel-mode target mode :server serv)
      (irc:user-mode target mode :server serv)))

;; LAST SEEN
(define-handler (join-event event) ()
  (setf (gethash (nick event) (last-seen module)) (get-universal-time)))

(define-handler (privmsg-event event) ()
  (setf (gethash (nick event) (last-seen module)) (get-universal-time)))

(defun format-time-since (secs)
  (multiple-value-bind (s m h dd yy) (decode-universal-time secs)
    (declare (ignore s))
    (setf yy (- yy 1) dd (- dd 1) m (- m 1) h (- h 1))
    (format NIL "~:[~D years, ~;~*~]~:[~D days, ~;~*~]~:[~D hours, ~;~*~]~D minutes" (= yy 0) yy (= dd 0) dd (= h 0) h m)))

(define-command last-seen (nick) (:documentation "Tell how long it has been since the bot last saw the requested nick.")
  (if (gethash nick (last-seen module))
      (respond event "I have last seen ~a ~a ago." nick (format-time-since (- (get-universal-time) (gethash nick (last-seen module)))))
      (respond event "I don't know anyone called ~a." nick)))

(define-command uptime () (:documentation "Report how long Colleen has been started up for (or more specifically, the Essentials module).")
  (respond event "Uptime: ~a" (format-time-since (- (get-universal-time) (startup-time module)))))









