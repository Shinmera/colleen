#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.essentials
  (:nicknames :co-essentials)
  (:use :cl :colleen :events :local-time :alexandria)
  (:shadow :shutdown))
(in-package :org.tymoonnext.colleen.mod.essentials)

(define-module essentials ()
    ((%startup :initform (get-universal-time) :accessor startup-time))
  (:documentation "A few essential bot and irc commands."))

(define-command reload () (:authorization T :documentation "Reload the bot configuration.")
  (when (auth-p (nick event))
    (respond event (fstd-message event :config-reload "Reloading bot configuration."))
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

(define-command help (&rest command-signature) (:documentation "Display help on a command.")
  (if command-signature
      (let* ((command-signature (format NIL "~{~a~^ ~}" command-signature))
             (handler (loop for handler across *cmd-priority-array*
                            do (when (cl-ppcre:scan (scanner handler) command-signature)
                                 (return handler)))))
        (if handler
            (typecase handler
              (group-handler
               (respond event "Closest match: [Group] ~a -- ~
                            ~:[No docstring available.~;~:*~a~]~%~
                            Commands in this group: ~:[None~;~:*~{~a~^, ~}~]"
                        (identifier handler) (docstring handler) (subcommands handler)))
              (command-handler
               (respond event "Closest match: [Command] ~a -- ~
                            ~:[No docstring available.~;~:*~a~]~%~
                            Arguments: ~a"
                        (identifier handler) (docstring handler) (arguments handler))))
            (respond event "?? Something went very wrong.")))
      (respond event "This is a Colleen IRC bot. This command lets you display help information on a specific command.")))

(define-command apropos (&rest command-signature) (:documentation "Display information on all matching commands.")
  (do-matching-command-handlers ((format NIL "~{~a~^ ~}" command-signature) handler)
    (respond event "~a" (apropos-command-handler handler))))

;; SYSTEM COMMANDS
(define-command version () (:documentation "Return version information.")
  (let ((versionstring (format NIL "Colleen v~a" (asdf:component-version (asdf:find-system :colleen)))))
    (when (uiop:directory-exists-p (merge-pathnames ".git" (asdf:system-source-directory :colleen)))
      (uiop:chdir (asdf:system-source-directory :colleen))
      (uiop:run-program "git fetch")
      (setf versionstring (format NIL "~a ~a (~a behind ~a ahead)" versionstring
                                  (string-trim '(#\Newline) (uiop:run-program "git rev-parse HEAD" :output :string))
                                  (string-trim '(#\Newline) (uiop:run-program "git rev-list HEAD..origin --count" :output :string))
                                  (string-trim '(#\Newline) (uiop:run-program "git rev-list origin..HEAD --count" :output :string)))))
    (respond event versionstring)))

(define-command quickload (&optional (system "colleen")) (:authorization T :documentation "Perform a ql:quickload.")
  (ql:quickload system)
  (respond event "System ~a loaded." system))

(defun pull-local-projects (event)
  (dolist (project (uiop:directory-files (merge-pathnames "quicklisp/local-projects/" (user-homedir-pathname))))
    (when (uiop:directory-pathname-p project)
      (uiop:chdir project)
      (uiop:run-program "git pull")
      (respond event "~a now at ~a."
               (car (last (pathname-directory project)))
               (string-trim '(#\Newline) (uiop:run-program "git rev-parse HEAD" :output :string))))))

(define-command pull-local-projects () (:authorization T :documentation "Perform a git-pull on all projects in Quicklisp's local-projects folder.")
  (pull-local-projects event))

(define-command update () (:authorization T :documentation "Stops all active modules (except essentials), performs a GIT pull on the project root, reloads and finally starts all previously active modules again.")
  (flet ((report (level message &rest formatargs)
           (apply #'respond event (format NIL "[update] ~a" message) formatargs)
           (apply #'v:log level :essentials.update message formatargs)))
    (report :info "Performing pull-local-projects...")
    (pull-local-projects event)
    (report :info "Reloading colleen...")
    (ql:quickload :colleen)
    (report :info "Reloading modules...")
    (dolist (system (asdf:registered-systems))
      (when (and (< 3 (length system)) (string-equal "co-" system :end2 3)
                 (module (subseq system 3)))
        (report :info "Loading ~a" system)
        (ql:quickload system)))
    (report :info "Update done.")))

;; MODULE COMMANDS
(define-group module :documentation "Manage bot modules.")

(define-command (module start) (module-name) (:authorization T :documentation "Start up a module.")
  (handler-case
      (progn (start-module module-name)
             (respond event "Module started."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (module stop) (module-name) (:authorization T :documentation "Stop a module.")
  (handler-case
      (progn (stop-module module-name)
             (respond event "Module stopped."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (module restart) (module-name) (:authorization T :documentation "Stop&Start a module.")
  (handler-case
      (progn (stop-module module-name)
             (start-module module-name)
             (respond event "Module restarted."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (module load) (module-system-name) (:authorization T :documentation "Initiate a load-module command. Note: Will not start any module and may stop running modules.")
  (handler-case
      (load-module module-system-name)
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (module storage save) (module-name) (:authorization T :documentation "Saves the storage of a module.")
  (handler-case
      (progn (save-storage (get-module module-name))
             (respond event "Module storage saved."))
    (error (err)
      (respond event "Error: ~a" err))))

(define-command (module storage load) (module-name) (:authorization T :documentation "Loads the storage of a module.")
  (handler-case
      (progn (load-storage (get-module module-name))
             (respond event "Module storage loaded."))
    (error (err)
      (respond event "Error: ~a" err))))

(defun format-module (stream module-name &rest rest)
  (declare (ignore rest))
  (format stream "~:[~a~;*~a~]" (active (get-module module-name)) module-name))

(define-command (module list) () (:documentation "List available modules.")
  (respond event "Modules [* activated]: ~{~/org.tymoonnext.colleen.mod.essentials::format-module/~^ ~}" (hash-table-keys *bot-modules*)))

(define-command (module help) (module-name) (:documentation "Show the docstring for a module.")
  (with-module (instance module-name)
    (respond event "~a: ~a" module-name (or (documentation (class-of instance) T) "No help available."))))

(define-command (module threads) (module-name) (:documentation "List threads that are currently active in this module.")
  (with-module (instance module-name)
    (let ((threads (alexandria:hash-table-keys (colleen:threads instance))))
      (if threads
          (respond event "~a runs the following threads: ~{~a~^, ~}" module-name
                   (mapcar #'(lambda (a) (format NIL "~:[~a~;*~a~]" (bordeaux-threads:thread-alive-p (gethash a (colleen:threads instance))) a)) threads))
          (respond event "~a does not have any running threads." module-name)))))

(define-command (module interrupt) (module-name) (:authorization T :documentation "Interrupt all the module threads.")
  (with-module (instance module-name)
    (let ((threads (alexandria:hash-table-keys (colleen:threads instance))))
      (if threads
          (loop for uid in threads
             for thread = (gethash uid (colleen:threads instance))
             do (if (bordeaux-threads:thread-alive-p thread)
                    (bordeaux-threads:interrupt-thread thread #'(lambda () (error 'module-stop)))
                    (remhash uid (threads module)))
               (respond event "Thread ~a interrupted." uid))
          (respond event "~a does not have any running threads." module-name)))))

(define-command (module kill) (module-name) (:authorization T :documentation "Kill all the module threads by forcefully releasing the lock first if possible.")
  (with-module (instance module-name)
    (let ((threads (alexandria:hash-table-keys (colleen:threads instance))))
      (if threads
          (loop for uid in threads
                for thread = (gethash uid (colleen:threads instance))
                do (when (bordeaux-threads:thread-alive-p thread)
                     (colleen::force-release-lock (lock instance))
                     (bordeaux-threads:destroy-thread thread))
                   (remhash uid (threads module))
                   (respond event "Thread ~a killed." uid))
          (respond event "~a does not have any running threads." module-name)))))

(define-command (module thread-stats) () (:authorization T :documentation "Command version of PRINT-MODULE-THREAD-STATS")
  (loop for v being the hash-values of *bot-modules*
        when (< 0 (hash-table-count (threads v)))
          do (respond event "~a"
                      (with-output-to-string (stream)
                        (format stream "~25a ~4a " v (hash-table-count (threads v)))
                        (loop for tv being the hash-values of (threads v)
                              do (format stream "~:[x~;.~]" (bt:thread-alive-p tv)))))))

(define-command (module sweep) (module-name) (:authorization T :documentation "Sweep dead threads from the module.")
  (multiple-value-bind (removed remaining) (sweep-module-threads module-name)
    (respond event "~d threads removed, ~d remaining." removed remaining)))

(define-command (module sweep-all) () (:authorization T :documentation "Sweep dead threads from all modules.")
  (sweep-all-module-threads)
  (respond event "Sweep done."))

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
  (setf (uc:config-tree :last-seen (nick event)) (get-universal-time)))

(define-handler (privmsg-event event) ()
  (setf (uc:config-tree :last-seen (nick event)) (get-universal-time)))

(defun format-time-since (secs)
  (multiple-value-bind (s m h dd yy) (decode-universal-time secs)
    (declare (ignore s))
    (setf yy (- yy 1) dd (- dd 1) m (- m 1) h (- h 1))
    (format NIL "~:[~D years, ~;~*~]~:[~D days, ~;~*~]~:[~D hours, ~;~*~]~D minutes" (= yy 0) yy (= dd 0) dd (= h 0) h m)))

(define-command last-seen (nick) (:documentation "Tell how long it has been since the bot last saw the requested nick.")
  (if (uc:config-tree :last-seen nick)
      (respond event "I have last seen ~a ~a ago." nick (format-time-since (- (get-universal-time) (uc:config-tree :last-seen nick))))
      (respond event "I don't know anyone called ~a." nick)))

(define-command uptime () (:documentation "Report how long Colleen has been started up for (or more specifically, the Essentials module).")
  (respond event "Uptime: ~a" (format-time-since (- (get-universal-time) (startup-time module)))))
