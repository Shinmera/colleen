#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :cl)
(defpackage org.tymoonnext.colleen
  (:use :cl :split-sequence :bordeaux-threads :alexandria)
  (:nicknames :colleen)
  ;; asdf-extra.lisp
  (:export
   #:module-system
   #:define-module-system
   #:load-module)
  ;; client.lisp
  (:export
   #:*servers*
   #:*current-server*
   
   #:server
   #:name
   #:auth-users
   #:channels
   #:nick
   #:host
   #:port
   #:username
   #:password
   #:realname
   
   #:get-server
   #:connect
   #:disconnect
   #:reconnect
   #:auth-p
   #:remove-from-auth
   #:add-to-auth
   #:process-event)
  ;; command-handler.lisp
  (:export
   #:*cmd-map*
   #:*cmd-priority-array*
   
   #:command-handler
   #:identifier
   #:pattern
   #:scanner
   #:arguments
   #:handler-function
   #:priority
   #:docstring
   
   #:generate-priority-cache
   #:command-handler
   #:remove-command-handler
   #:set-command-function
   #:apropos-command-handler
   #:do-matching-command-handlers
   #:dispatch-command
   #:stop-command
   #:skip-handler
   #:rematch-handler
   #:recheck-arguments
   #:retry-handler
   #:simulate-command
   #:relay-command
   
   #:group-handler
   #:subcommands
   
   #:define-group
   #:define-command)
  ;; conditions.lisp
  (:export
   #:module-error
   #:invalid-arguments
   #:command
   #:argslist
   #:expected
   
   #:not-authorized
   #:events
   
   #:network-error
   #:failed-server
   
   #:disconnect
   
   #:connection-failed
   #:initial-error
   
   #:ping-timeout
   
   #:module-stop
   
   #:module-system-not-found
   #:name
   
   #:message-too-long
   #:message
   
   #:implicit-group-definition
   #:group)
  ;; config.lisp
  (:export
   #:*config-file*
   #:*config-directory*
   #:*default-config-file*
   #:*config*
   
   #:load-config
   #:save-config
   #:bot-config
   #:server-config)
  ;; event-handler.lisp
  (:export
   #:*evt-map*
   #:*evt-priority-map*
   
   #:event-handler
   #:event-type
   #:identifier
   #:handler-function
   #:priority
   #:docstring
   
   #:generate-handler-priority-cache
   #:event-handler
   #:remove-event-handler
   #:apropos-event-handler
   #:set-handler-function
   #:dispatch
   #:define-handler)
  ;; event-priority.lisp
  (:export
   #:*priority-names*
   #:*priority-nums*
   #:priority-name
   #:priority-num)
  ;; event.lisp
  (:export
   #:*event-map*
   
   #:event
   #:server
   #:prefix
   #:arguments
   #:cancelled
   #:dispatched
   
   #:user-event
   #:username
   #:hostmask
   #:nick
   
   #:channel-event
   #:channel
   
   #:command-event
   #:command
   #:cmd-args
   
   #:generated-command-event
   #:output-stream
   
   #:send-event
   #:nick
   #:channel
   #:message
   
   #:respond
   #:define-event
   #:make-event)
  ;; globals.lisp
  (:export
   #:*debugger*
   #:*irc-message-limit*
   #:*privmsg-line-limit*
   #:*server-encoding*)
  ;; launcher.lisp
  (:export
   #:startup
   #:shutdown)
  ;; module-storage.lisp
  (:export
   #:with-module-storage
   #:module-config-path
   #:save-storage
   #:load-storage)
  ;; module.lisp
  (:export
   #:*bot-modules*
   #:*current-module*
   
   #:module
   #:active
   #:threads
   #:lock
   #:storage
   
   #:start
   #:stop
   #:module-thread
   #:stop-module-thread
   #:remove-module-thread
   #:with-module
   #:with-module-thread
   #:with-module-lock
   #:print-module-thread-stats
   #:sweep-module-threads
   #:sweep-all-module-threads
   #:to-module-name
   #:get-module
   #:name
   #:get-current-module
   #:get-current-module-name
   #:define-module
   #:start-module
   #:stop-module
   #:skip
   #:force
   #:retry)
  ;; irc-codes.lisp
  (:export
   #:*reply-code-map*
   #:reply->keyword)
  ;; time-handler.lisp
  (:export
   #:*timer-map*
   
   #:time-handler
   #:identifier
   #:timer-type
   #:arguments
   #:handler-function
   #:launcher-function
   #:schedulings
   #:handler-lock
   #:docstring
   
   #:time-handler
   #:stop-time-handler
   #:remove-time-handler
   #:set-timer-function
   #:apropos-time-handler
   
   #:parse-delay
   #:schedule-timer
   #:reschedule-timer
   
   #:simple-launcher
   #:module-launcher
   #:define-timer)
  ;; toolkit.lisp
  (:export
   #:format-message
   #:standard-message
   #:fstd-message

   #:mirc-color->name
   #:strip-colors
   #:color-entities

   #:break-string)
  (:shadow time trace))

(defpackage org.tymoonnext.colleen.commands
  (:use :cl)
  (:import-from :colleen :*privmsg-line-limit* :*current-server* :*irc-message-limit* :*server-encoding* :name :channels :message-too-long :break-string)
  (:nicknames :irc)
  (:export
   :send-raw
   :pass
   :nick
   :user
   :server
   :oper
   :quit
   :squit
   :join
   :part
   :user-mode
   :channel-mode
   :topic
   :names
   :list
   :invite
   :kick
   :version
   :stats
   :links
   :time
   :connect
   :trace
   :admin
   :info
   :privmsg
   :notice
   :broadcast
   :who
   :whois
   :whowas
   :kill
   :ping
   :pong
   :error
   :away
   :rehash
   :restart
   :summon
   :users
   :wallops
   :userhost
   :ison)
  (:shadow :time :trace :restart :list :error))

(defpackage org.tymoonnext.colleen.events
  (:use :cl :colleen)
  (:nicknames :events)
  (:export
   ;; Event classes
   :event
   :user-event
   :channel-event
   :command-event
   :send-event
   :ping-event
   :pong-event
   :nick-event
   :privmsg-event
   :join-event
   :part-event
   :quit-event
   :mode-event
   :topic-set-event
   :kick-event
   :notice-event
   :welcome-event
   :your-host-event
   :created-event
   :myinfo-event
   :bounce-event
   :isupport-event
   :your-id-event
   :save-nick-event
   :trace-link-event
   :trace-connecting-event
   :trace-handshake-event
   :trace-unknown-event
   :trace-operator-event
   :trace-user-event
   :trace-server-event
   :trace-service-event
   :trace-newtype-event
   :trace-class-event
   :stats-linkinfo-event
   :stats-commands-event
   :stats-cline-event
   :stats-nline-event
   :stats-iline-event
   :stats-kline-event
   :stats-yline-event
   :stats-end-event
   :umode-event
   :servlist-event
   :servlist-end-event
   :statsl-line-event
   :stats-uptime-event
   :stats-oline-event
   :stats-hline-event
   :luser-client-event
   :luser-op-event
   :luser-unknown-event
   :luser-channels-event
   :luser-me-event
   :admin-me-event
   :admin-loc1-event
   :admin-loc2-event
   :admin-email-event
   :trace-log-event
   :trace-end-event
   :try-again-event
   :local-users-event
   :global-users-event
   :away-event
   :userhost-event
   :ison-event
   :un-away-event
   :now-away-event
   :whois-user-event
   :whois-server-event
   :whois-operator-event
   :whowas-user-event
   :who-end-event
   :whois-idle-event
   :whois-end-event
   :whois-channels-event
   :list-start-event
   :list-event
   :list-end-event
   :channel-mode-event
   :uniqop-event
   :whois-account-event
   :no-topic-event
   :topic-event
   :topic-who-time-event
   :inviting-event
   :summoning-event
   :invited-event
   :invitelist-event
   :invitelist-end-event
   :exceptlist-event
   :exceptlist-end-event
   :version-event
   :who-reply-event
   :namreply-event
   :links-event
   :links-end-event
   :names-end-event
   :banlist-event
   :banlist-end-event
   :whowas-end-event
   :info-event
   :motd-event
   :info-end-event
   :motd-start-event
   :motd-end-event
   :spam-event
   :youre-oper-event
   :rehashing-event
   :youre-service-event
   :time-event
   :users-start-event
   :users-event
   :users-end-event
   :nousers-event
   :host-hidden-event
   :unknow-nerror-event
   :nosuch-nick-event
   :nosuch-server-event
   :nosuch-channel-event
   :cannot-send-to-chan-event
   :too-many-channels-event
   :was-no-such-nick-event
   :too-many-targets-event
   :no-such-service-event
   :no-origin-event
   :no-recipient-event
   :no-text-to-send-event
   :no-top-level-event
   :wild-top-level-event
   :bad-mask-event
   :too-many-matches-event
   :unknown-command-event
   :no-motd-event
   :no-admininfo-event
   :file-error-event
   :no-nickname-given-event
   :erroneusnickname-event
   :nickname-in-use-event
   :nick-collision-event
   :unavail-resource-event
   :user-not-in-channel-event
   :not-on-channel-event
   :user-on-channel-event
   :no-login-event
   :summon-disabled-event
   :users-disabled-event
   :not-implemented-event
   :not-registered-event
   :need-more-params-event
   :already-registered-event
   :no-perm-for-host-event
   :password-mismatch-event
   :youre-banned-creep-event
   :key-set-event
   :channel-full-event
   :unknown-mode-event
   :inviteonly-chan-event
   :banned-from-chan-event
   :bad-channel-key-event
   :bad-channel-mask-event
   :no-channel-modes-event
   :banlist-full-event
   :no-privileges-event
   :chan-privs-needed-event
   :cant-kill-server-event
   :restricted-event
   :unique-privs-needed-event
   :no-operhost-event
   :mode-unknown-flag-event
   :users-dont-match-event
   :vworld-warn-event
   :disabled-event
   :remote-pfx-event
   :pfx-unroutable-event
   :traceroute-hop-event
   :traceroute-start-event
   :mode-change-warn-event
   :chanredir-event
   :servmode-event
   :other-umode-event
   :generic-end-event
   :whowas-details-event
   :whois-secure-event
   :unknown-modes-event
   :cannot-set-modes-event
   :luser-staff-event
   :time-on-server-event
   :networks-event
   :your-language-event
   :language-event
   :whois-staff-event
   :whois-language-event
   :modlist-event
   :modlist-end-event
   :help-start-event
   :help-txt-event
   :help-end-event
   :etrace-full-event
   :etrace-event
   :knock-event
   :knock-dlvr-event
   :too-many-knock-event
   :chan-open-event
   :knock-on-chan-event
   :knock-disabled-event
   :target-umodeg-event
   :target-notify-event
   :umodeg-msg-event
   :omotd-start-event
   :omotd-event
   :omotd-end-event
   :no-privs-event
   :testmark-event
   :testline-event
   :no-testline-event
   :cannot-change-umode-event
   :cannot-change-chanmode-event
   :cannot-change-servermode-event
   :cannot-send-to-nick-event
   :unknown-server-mode-event
   :server-mode-lock-event
   :bad-char-encoding-event
   :too-many-languages-event
   :no-language-event
   :text-too-short-event

   ;; Event accessors
   :reason
   :server1
   :server2
   :new-nick
   :target
   :targets
   :mode
   :parameter
   :topic
   :server-name
   :version
   :user-modes
   :chan-modes
   :channel-modes-with-params
   :user-modes-with-params
   :server-modes
   :server-modes-with-params
   :params
   :link
   :destination
   :next-server
   :vprotocol-version
   :link-uptime-in-seconds
   :backstream-sendq
   :upstream-sendq
   :group
   :connection-address
   :oper
   :user
   :serv
   :ints
   :intc
   :hostname
   :service
   :service-type
   :active-type
   :newtype
   :client-name
   :class-count
   :linkname
   :sendq
   :sent-msgs
   :sent-bytes
   :recvd-msgs
   :rcvd-bytes
   :invitee
   :sender
   :time-open
   :cmd-count
   :byte-count
   :remote-count
   :host2
   :ping-freq
   :connect-freq
   :max-sendq
   :query
   :user-mode-params
   :mask
   :server-type
   :hopcount
   :maxdepth
   :int
   :admin-location
   :email-address
   :file
   :logfile
   :debug-level
   :real-name
   :server-info
   :privileges
   :seconds
   :users
   :visible
   :mode-params
   :nickname
   :authname
   :invitemask
   :exceptionmask
   :comments
   :name-type
   :nickinfo
   :banid
   :time-left
   :config-file
   :time-string
   :service-name
   :resource
   :mode-char
   :chan-char
   :hop
   :address
   :usec-ping
   :target-trace
   :target-fqdn
   :target-address
   :max-hops
   :old-chan
   :new-chan
   :modes
   :parameters
   :who-type
   :staff-online-count
   :nanoseconds
   :timezone
   :flags
   :through-name
   :hops
   :code
   :revision
   :maintainer
   :language
   :codes
   :modechar
   :charset
   :max-langs
   :language-code))
