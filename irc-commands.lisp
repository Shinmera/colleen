#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.commands)

(defun raw (server message &rest formatargs)
  "Send a raw message over the server socket."
  (let ((message (apply #'format NIL message formatargs)))
    (when (< *irc-message-limit* (length (flexi-streams:string-to-octets message :external-format *server-encoding*)))
      (warn 'message-too-long :message message))
    (with-accessors ((stream colleen::socket-stream)) server
      (write-string message stream)
      (write-char #\Return stream)
      (write-char #\Linefeed stream)
      (finish-output stream))
    message))

(defun send-raw (message &key (server *current-server*))
  "Send a raw IRC command."
  (v:trace (name server) "Sending RAW message: ~a" message)
  (raw server message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACCORDING TO RFC 1459

;; CONNECTION REGISTRATION
(defun pass (password &key (server *current-server*))
  "Send a password message to the server."
  (v:trace (name server) "Sending password ~a" password)
  (raw server "PASS ~a" password))

(defun nick (new-nick &key (server *current-server*))
  "Change the current nick-name."
  (v:trace (name server) "Requesting nick change to ~a" new-nick)
  (raw server "NICK ~a" new-nick))

(defun user (username hostname servername realname &key (server *current-server*))
  "Send the USER command to set user params."
  (v:trace (name server) "Sending USER command ~a ~a ~a ~a" username hostname servername realname)
  (raw server "USER ~a ~a ~a :~a" username hostname servername realname))

(defun server (servername hopcount info &key (server *current-server*))
  "Send the SERVER command to identify as a server."
  (v:trace (name server) "Sending SERVER command ~a ~a ~a" servername hopcount info)
  (raw server "SERVER ~a ~a :~a" servername hopcount info))

(defun oper (username password &key (server *current-server*))
  "Request OPER privileges."
  (v:trace (name server) "Requesting OPER ~a/~a" username password)
  (raw server "OPER ~a ~a" username password))

(defun quit (&key quit-message (server *current-server*))
  "Quit a server."
  (v:trace (name server) "Sending QUIT: ~a" quit-message)
  (raw server "QUIT~@[ :~a~]" quit-message)
  (setf (channels server) NIL))

(defun squit (remote-server quit-message &key (server *current-server*))
  "Disconnect a remote server from the network."
  (v:trace (name server) "Sending SQUIT to ~a: ~a" remote-server quit-message)
  (raw server "SQUIT ~a :~a" remote-server quit-message))

;; CHANNEL OPERATIONS
(defun join (channel &key key (server *current-server*))
  "Join a channel."
  (unless (find channel (channels server) :test #'equalp)
    (raw server "JOIN ~a~@[ ~a~]" channel key)
    (push channel (channels server))
    (v:trace (name server) "Joining ~a" channel)))

(defun part (channel &key (server *current-server*))
  "Part a channel."
  (when (find channel (channels server) :test #'equalp)
    (setf (channels server)
          (delete channel (channels server) :test #'equalp))
    (v:trace (name server) "Parting ~a" channel)
    (raw server "PART ~a" channel)))

(defun user-mode (user mode &key (server *current-server*))
  "Change the mode of a user."
  (v:trace (name server) "Changing mode of ~a to ~a" user mode)
  (raw server "MODE ~a ~a" user mode))

(defun channel-mode (channel mode &key extra (server *current-server*))
  "Change the mode of a channel."
  (v:trace (name server) "Changing mode of ~a to ~a ~a" channel mode extra)
  (raw server "MODE ~a ~a~@[ ~a~]" channel mode extra))

(defun topic (channel &key topic (server *current-server*))
  "Get or set the topic of a channel."
  (if topic
      (v:trace (name server) "Setting topic of ~a: ~a" channel topic)
      (v:trace (name server) "Requesting topic of ~a." channel))
  (raw server "TOPIC ~a~@[ :~a~]" channel topic))

(defun names (channel &key (server *current-server*))
  "Request nick list of a channel."
  (v:trace (name server) "Requesting nick list of channel ~a" channel)
  (raw server "NAMES ~a" channel))

(defun list (&key channel (server *current-server*))
  "Request channel list or info."
  (v:trace (name server) "Request channel list (~a)" channel)
  (raw server "LIST~@[ ~a~]" channel))

(defun invite (nick channel &key (server *current-server*))
  "Invite a user to a channel."
  (v:trace (name server) "Inviting ~a to ~a" nick channel)
  (raw server "INVITE ~a ~a" nick channel))

(defun kick (channel nick &key reason (server *current-server*))
  "Kick a user from a channel."
  (v:trace (name server) "Kicking ~a from ~a (~a)" nick channel reason)
  (raw server "KICK ~a ~a~@[ :~a~]" channel nick reason))

;; SERVER QUERIES AND COMMANDS
(defun version (&key remote-server (server *current-server*))
  "Request the version string of a server."
  (v:trace (name server) "Requesting VERSION from ~a." remote-server)
  (raw server "VERSION~@[ ~a~]" remote-server))

(defun stats (&key query remote-server (server *current-server*))
  "Request stats information from a server."
  (v:trace (name server) "Requesting STATS ~a from ~a." query remote-server)
  (raw server "STATS~@[ ~a~@[ ~a~]~]" query remote-server))

(defun links (&key remote-server mask (server *current-server*))
  "Request a links list matching the provided mask."
  (v:trace (name server) "Requesting LINKS ~a from ~a." mask remote-server)
  (raw server "LINKS~:[~;~@[ ~a~] ~:*~:*~a~]" mask remote-server))

(defun time (&key remote-server (server *current-server*))
  "Request local-time from a server."
  (v:trace (name server) "Requesting TIME from ~a." remote-server)
  (raw server "TIME~@[ ~a~]" remote-server))

(defun connect (target-server &key port remote-server (server *current-server*))
  "Force a server to establish a new connection immediately."
  (v:trace (name server) "Requesting CONNECT to ~a:~a from ~a." target-server port remote-server)
  (raw server "CONNECT ~a~@[ ~a~@[ ~a~]~]" target-server port remote-server))

(defun trace (&key remote-server (server *current-server*))
  "Request a route trace to a server."
  (v:trace (name server) "Requesting TRACE to ~a." remote-server)
  (raw server "TRACE~@[ ~a~]" remote-server))

(defun admin (&key remote-server (server *current-server*))
  "Request the name of the admin of a server."
  (v:trace (name server) "Requesting ADMIN from ~a." remote-server)
  (raw server "ADMIN~@[ ~a~]" remote-server))

(defun info (&key remote-server (server *current-server*))
  "Request INFO of a server."
  (v:trace (name server) "Requesting INFO from ~a." remote-server)
  (raw server "INFO~@[ ~a~]" remote-server))

;; SENDING MESSAGES
(defun privmsg (target message &key (server *current-server*) (line-limit *privmsg-line-limit*) (length-limit 350) (break-string T))
  "Send a PRIVMSG."
  (when message
    (when (and break-string (< length-limit (length message)))
      (warn 'message-too-long :message message :limit length-limit)
      (setf message (break-string message :limit length-limit)))
    (v:trace (name server) "Sending privmsg to ~a: ~a" target message)
    (colleen:dispatch (make-instance 'colleen:send-event :server server :nick (colleen:nick server) :channel target :message message))
    (loop for message in (split-sequence:split-sequence #\Newline message)
          for i from 0 below line-limit
          collect (raw server "PRIVMSG ~a :~a" target message))))

(defun notice (target message &key (server *current-server*))
  "Send a NOTICE."
  (when message
    (v:trace (name server) "Sending notice to ~a: ~a" target message)
    (raw server "NOTICE ~a :~a" target message)))

(defun broadcast (message &key (all-servers T))
  "Broadcast a message across all channels or even all servers."
  (when message
    (v:trace :colleen.client "Broadcasting message (all-servers ~a): ~a" all-servers message)
    (alexandria:maphash-values 
     #'(lambda (server)
         (dolist (channel (channels server))
           (privmsg channel message :server server)))
     colleen:*servers*)))

;; USER-BASED QUERIES
(defun who (&key name opers-only (server *current-server*))
  "Request al ist of matching users."
  (v:trace (name server) "Requesting WHO (opers ~a): ~a" opers-only name)
  (raw server "WHO~@[ ~a~@[ o~]~]" name opers-only))

(defun whois (nickmask &key remote-server (server *current-server*))
  "Request WHOIS info on a user."
  (v:trace (name server) "Requesting WHOIS on ~a from ~a." nickmask remote-server)
  (raw server "WHOIS~@[ ~a~] ~a" remote-server nickmask))

(defun whowas (nick &key count remote-server (server *current-server*))
  "Request the NICK history of a user."
  (v:trace (name server) "Requesting WHOWAS (~a) on ~a from ~a" count nick remote-server)
  (raw server "WHOWAS ~a~@[ ~a~@[ ~a~]~]" nick count remote-server))

;; MISCELLANEOUS MESSAGES
(defun kill (nick reason &key (server *current-server*))
  "Kill the requested user from the server."
  (v:trace (name server) "Sending KILL for ~a: ~a" nick reason)
  (raw server "KILL ~a :~a" nick reason))

(defun ping (target &key (server *current-server*))
  "Send a PING message to the target."
  (v:trace (name server) "Sending PING to ~a" target)
  (raw server "PING ~a" target))

(defun pong (target &key (server *current-server*))
  "Send a PONG message to the target."
  (v:trace (name server) "Sending PONG to ~a" target)
  (raw server "PONG ~a" target))

(defun error (error-message &key (server *current-server*))
  "Send an ERROR message to the network."
  (v:trace (name server) "Sending ERROR: ~a" error-message)
  (raw server "ERROR :~a" error-message))

;; OPTIONAL MESSAGES
(defun away (&key message (server *current-server*))
  "Set your AWAY status"
  (v:trace (name server) "Sending AWAY: ~a" message)
  (raw server "AWAY :~a" message))

(defun rehash (&key (server *current-server*))
  "Request a REHASH of the configuration."
  (v:trace (name server) "Sending REHASH.")
  (raw server "REHASH"))

(defun restart (&key (server *current-server*))
  "Request a RESTART of the server."
  (v:trace (name server) "Sending RESTART.")
  (raw server "RESTART"))

(defun summon (user &key remote-server (server *current-server*))
  "Attempt to summon a user from a server."
  (v:trace (name server) "Sending SUMMON of ~a to ~a" user remote-server)
  (raw server "SUMMON ~a~@[ ~a~]" user remote-server))

(defun users (&key remote-server (server *current-server*))
  "Request a list of connected users on a server."
  (v:trace (name server) "Requesting USERS from ~a." remote-server)
  (raw server "USERS~@[ ~a~]" remote-server))

(defun wallops (message &key (server *current-server*))
  "Send a message to all opers."
  (v:trace (name server) "Sending WALLOPS: ~a" message)
  (raw server "WALLOPS :~a" message))

(defun userhost (nicks &key (server *current-server*))
  "Request information about up to 5 nicks."
  (unless (listp nicks) (setf nicks (cl:list nicks)))
  (assert (< (length nicks) 5) () "Maximum 5 nicks allowed by RFC. Attempted to request: ~a" nicks)
  (v:trace (name server) "Requesting USERHOST about ~a" nicks)
  (raw server "USERHOST ~{~a~^ ~}" nicks))

(defun ison (nicks &key (server *current-server*))
  "Check if nicks are online, using the ISON command."
  (unless (listp nicks) (setf nicks (cl:list nicks)))
  (v:trace (name server) "Requesting ISON about: ~a" nicks)
  (raw server "ISON ~{~a~^ ~}" nicks))

