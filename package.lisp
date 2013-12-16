#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :cl)
(defpackage org.tymoonnext.colleen
  (:use :cl :drakma :cl-json :split-sequence :lquery :bordeaux-threads :alexandria)
  (:nicknames :colleen)
  (:export 
   ;; GLOBALS
   :*servers*
   :*bot-modules*
   :*conf-file*
   :*current-server*
   ;; CONDITIONS
   :invalid-arguments
   :network-error
   :disconnect
   :ping-timeout
   :connection-failed
   :nickname-in-use
   :not-authorized
   ;; REPLY-CODES
   :reply->keyword
   ;; CONFIG
   :load-config
   :config
   :config-tree
   :server-config
   :format-message
   :standard-message
   :fstd-message
   ;; EVENTS
   :event
   :server
   :prefix
   :arguments
   :define-event
   :user-event
   :username
   :hostmask
   :nick
   :channel-event
   :channel
   :command-event
   :command
   :cmd-args
   :send-event
   :respond
   ;; LAUNCHER
   :startup
   :shutdown
   ;; MODULE
   :module
   :active
   :start
   :stop
   :command
   :docu
   :cmd-args
   :cmd-fun
   :add-group
   :add-group-command
   :add-command
   :add-handler
   :dispatch
   :get-module
   :display-help
   :define-module
   :define-group
   :define-command
   :define-handler
   ;; SERVER
   :name
   :auth-users
   :channels
   :nick
   :host
   :port
   :username
   :password
   :realname
   :get-server
   :connect
   :disconnect
   :reconnect
   ;; COLLEEN
   :start-module
   :stop-module
   :auth-p
   :remove-from-auth
   :add-to-auth
   :process-event
   )
  (:shadow time trace))

(defpackage org.tymoonnext.colleen.commands
  (:use :cl)
  (:import-from :colleen :*current-server* :name :channels)
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
  (:use :cl)
  (:import-from :colleen :event :define-event :user-event :channel-event)
  (:nicknames :events)
  (:export
   :welcome-event
   :ping-event
   :pong-event
   :server1
   :server2
   :nick-event
   :old-nick
   :privmsg-event
   :message
   :join-event
   :part-event
   :quit-event
   :reason
   :mode-event
   :target
   :mode
   :parameter
   :topic-event
   :topic
   :kick-event
   :reason
   :whois-user-event
   :realname
   :whois-channels-event
   :channels
   :whois-server-event
   :hostname
   :description
   :whois-idle-event
   :idle-time
   :signon-time
   :whois-end-event
   :motd-event
   :motd-end-event
   :notice-event))
