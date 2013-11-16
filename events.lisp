#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defun make-event (event-name server prefix arguments)
  (let ((class (gethash event-name *event-map*)))
    (if class
        (make-instance class :server server :prefix prefix :arguments arguments)
        (v:warn (name server) "Inexistent event: ~a" event-name))))

(defclass event ()
  ((%server :initarg :server :reader server)
   (%prefix :initarg :prefix :reader prefix)
   (%args :initarg :arguments :reader arguments))
  (:documentation "Base event class."))

(defmacro arguments-bind ((&rest vars) expression &body body)
  (let ((ignores) (stringvar))
    ;; Change BODY to REST and remember var name.
    (let ((stringpos (position '&string vars)))
      (when stringpos
        (setf stringvar (nth (1+ stringpos) vars))
        (setf (nth stringpos vars) '&rest)))
    ;; Always add a REST
    (unless (find '&rest vars)
      (let ((gensym (gensym "REST")))
        (setf vars (append vars (list '&rest gensym)))
        (push gensym ignores)))
    ;; Filter out NILs
    (setf vars (mapcar #'(lambda (var) 
                           (if var var (let ((gensym (gensym)))
                                         (push gensym ignores)
                                         gensym))) vars))
    ;; Put together.
    `(destructuring-bind ,vars ,expression
       (declare (ignore ,@ignores))
       ,@(when stringvar (list `(setf ,stringvar (format NIL "~{~a~^ ~}" ,stringvar))))
       ,@body)))

(defmacro define-event (name event-or-code (&rest superclasses) &optional structure class-options)
  (let ((eventvar (gensym "EVENT"))
        (restvar (gensym "REST"))
        (varlist (remove-if-not #'(lambda (var) (and var (not (find var '(&rest &string &optional))))) structure)))
    `(progn
       (defclass ,name (,@superclasses) 
         ,(loop for var in varlist
             collect `(,(intern (format NIL "%~a" var)) :initarg ,(intern (format NIL "~a" var) "KEYWORD") :initform NIL :reader ,var)) 
         (,@class-options))
       ,(when event-or-code
              `(setf (gethash ,event-or-code *event-map*) ',name))
       ,(when structure
          `(progn (defmethod initialize-instance :after ((,eventvar ,name) &rest ,restvar)
                    (declare (ignore ,restvar))
                    (arguments-bind (,@structure) (arguments ,eventvar)
                      ,@(loop for var in varlist
                           collect `(setf (slot-value ,eventvar ',(find-symbol (format NIL "%~a" var))) ,var))))
                  (defmethod print-object ((,eventvar ,name) stream)
                    (print-unreadable-object (,eventvar stream :type T)
                      (format stream ,(format NIL "~{~a: ~~a~^ ~}" varlist)
                              ,@(mapcar #'(lambda (var) `(,var ,eventvar)) varlist)))))))))

(defclass user-event (event)
    ((%username :initarg :username :reader username)
     (%hostmask :initarg :hostmask :reader hostmask)
     (%nickname :initarg :nick :reader nick))
    (:documentation "Events related to users."))

(defmethod initialize-instance :after ((event user-event) &rest rest)
  (declare (ignore rest))
  (cl-ppcre:register-groups-bind (nick username hostmask) (*user-regex* (prefix event))
    (setf (slot-value event '%username) username
          (slot-value event '%hostmask) hostmask
          (slot-value event '%nickname) nick))
  (unless (slot-boundp event '%nickname)
    (setf (slot-value event '%username) (prefix event)
          (slot-value event '%hostmask) (prefix event)
          (slot-value event '%nickname) (prefix event))))

(defmethod print-object ((event user-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "NICK: ~a USER: ~a HOST: ~a" (nick event) (username event) (hostmask event))))

(define-event channel-event NIL (user-event)
    (channel)
    (:documentation "Events for channel commands."))

(defclass command-event (channel-event)
    ((%command :initarg :command :accessor command)
     (%cmd-args :initarg :cmd-args :accessor cmd-args))
    (:documentation "Event for commands."))

(defgeneric respond (event message &rest format-args)
  (:documentation "Respond to an event origin with the given message."))

(defmethod respond ((event user-event) message &rest format-args)
  (let ((message (apply #'format NIL message format-args)))
    (v:debug (name (server event)) "Replying to ~a: ~a" event message)
    (irc:privmsg (nick event) message :server (server event))))

(defmethod respond ((event channel-event) message &rest format-args)
  (let ((message (apply #'format NIL message format-args)))
    (v:debug (name (server event)) "Replying to ~a: ~a" event message)
    (irc:privmsg (channel event) message :server (server event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event welcome-event :RPL_WELCOME (event) ()
    (:documentation "Event on successful logon."))

(define-event ping-event :PING (event) 
    (server1 &optional server2)
    (:documentation "Event on a PING request."))

(define-event nick-event :NICK (user-event)
    (old-nick)
    (:documentation "Event on nick change."))

(define-event privmsg-event :PRIVMSG (channel-event)
    (channel message)
    (:documentation "Privmsg event."))

(defmethod initialize-instance :after ((event privmsg-event) &rest rest)
  (declare (ignore rest))
  (with-slots ((channel %channel) (message %message)) event
    (setf channel (first (arguments event))
          message (second (arguments event)))
    (unless (char= (aref channel 0) #\#)
      (setf channel (nick event)))))

(define-event join-event :JOIN (channel-event) ()
    (:documentation "User join event."))

(define-event part-event :PART (channel-event) ()
    (:documentation "User part event."))

(define-event quit-event :QUIT (user-event)
    (reason)
    (:documentation "User quit event."))

(define-event mode-event :MODE (user-event)
    (target mode &optional parameter)
    (:documentation "Mode change event."))

(define-event topic-event :TOPIC (channel-event)
    (channel topic)
    (:documentation "Topic set event."))

(define-event kick-event :KICK (channel-event)
    (channel target reason)
    (:documentation "User kick event."))

(define-event whois-user-event :RPL_WHOISUSER (event)
    (NIL nick username hostmask NIL realname)
    (:documentation "Event on WHOISUSER response."))

(define-event whois-channels-event :RPL_WHOISCHANNELS (event)
    (NIL nick &rest channels)
    (:documentation "Event on WHOISCHANNELS response."))

(define-event whois-server-event :RPL_WHOISSERVER (event)
    (NIL nick hostname description)
    (:documentation "Event on WHOISSERVER response."))

(define-event whois-idle-event :RPL_WHOISIDLE (event)
    (NIL nick idle-time signon-time)
    (:documentation "Event on WHOISIDLE response."))

(define-event whois-end-event :RPL_ENDOFWHOIS (event)
    (NIL nick)
    (:documentation "Event on ENDOFWHOIS response."))

(define-event motd-event :RPL_MOTD (event)
    (NIL message)
    (:documentation "Event on a MOTD line."))

(define-event motd-end-event :RPL_ENDOFMOTD (event) ()
    (:documentation "Event on a ENDOFMOTD line."))

(define-event notice-event :NOTICE (user-event)
    (NIL message)
    (:documentation "Notice message."))

#|
(1 :rpl_welcome)
(2 :rpl_yourhost)
(3 :rpl_created)
(4 :rpl_myinfo)
(5 :rpl_isupport) 
(10 :rpl_bounce)
(15 :rpl_map) ; From ircd 2.11 source
(17 :rpl_mapend) ; From ircd 2.11 source
(18 :rpl_mapstart) ; From ircd 2.11 source
(20 :rpl_hello) ; From ircd 2.11 source
(42 :rpl_yourid) ; From ircd 2.11 source
(43 :rpl_savenick) ; From ircd 2.11 source
(200 :rpl_tracelink)
(201 :rpl_traceconnecting)
(202 :rpl_tracehandshake)
(203 :rpl_traceunknown)
(204 :rpl_traceoperator)
(205 :rpl_traceuser)
(206 :rpl_traceserver)
(207 :rpl_traceservice)
(208 :rpl_tracenewtype)
(209 :rpl_traceclass)
(210 :rpl_tracereconnect)
(211 :rpl_statslinkinfo)
(212 :rpl_statscommands)
(213 :rpl_statscline)
(214 :rpl_statsnline)
(215 :rpl_statsiline)
(216 :rpl_statskline)
(217 :rpl_statsqline)
(218 :rpl_statsyline)
(219 :rpl_endofstats)
(221 :rpl_umodeis)
(225 :rpl_statsdline) ; Seen in dancer ircd source
(227 :rpl_option) ; Seen in dancer ircd source
(228 :rpl_endoptions) ; Seen in dancer ircd source
(231 :rpl_serviceinfo)
(232 :rpl_endofservices)
(233 :rpl_service)
(234 :rpl_servlist)
(235 :rpl_servlistend)
(240 :rpl_statsvline)
(241 :rpl_statslline)
(242 :rpl_statsuptime)
(243 :rpl_statsonline)
(244 :rpl_statshline)
(245 :rpl_statssline) ; The RFC says 244 but I believe that was a typo.
(246 :rpl_statsping)
(247 :rpl_statsbline)
(248 :rpl_statsuline) ; Seen in dancer ircd source
(249 :rpl_statsdebug) ; Seen in dancer ircd source
(250 :rpl_statsdline)
(251 :rpl_luserclient)
(252 :rpl_luserop)
(253 :rpl_luserunknown)
(254 :rpl_luserchannels)
(255 :rpl_luserme)
(256 :rpl_adminme)
(257 :rpl_adminloc1)
(258 :rpl_adminloc2)
(259 :rpl_adminemail)
(261 :rpl_tracelog)
(262 :rpl_traceend)
(263 :rpl_tryagain)
(265 :rpl_localusers) ; Seen in dancer ircd source
(266 :rpl_globalusers) ; Seen in dancer ircd source
(268 :rpl_mode) ; Seen in dancer ircd source
(269 :rpl_endmode) ; Seen in dancer ircd source
(271 :rpl_sitelist) ; Seen in dancer ircd source
(272 :rpl_endsitelist) ; Seen in dancer ircd source
(290 :rpl_clientcapab) ; Seen in dancer ircd source
(292 :rpl_noservicehost)
(300 :rpl_none)
(301 :rpl_away)
(302 :rpl_userhost)
(303 :rpl_ison)
(304 :rpl_away)
(305 :rpl_unaway)
(306 :rpl_noaway)
(311 :rpl_whoisuser)
(312 :rpl_whoisserver)
(313 :rpl_whoisoperator)
(314 :rpl_whowasuser)
(315 :rpl_endofwho)
(316 :rpl_whoischanop)
(317 :rpl_whoisidle)
(318 :rpl_endofwhois)
(319 :rpl_whoischannels)
(320 :rpl_whoisidentified) ; Seen in dancer ircd source
(321 :rpl_liststart)
(322 :rpl_list)
(323 :rpl_listend)
(324 :rpl_channelmodeis)
(325 :rpl_uniqopis)
(326 :rpl_whoisoperprivs) ; Seen in dancer ircd source
(327 :rpl_whoisrealhost) ; Seen in dancer ircd source
(329 :rpl_creationtime) ; Seen in dancer ircd source
(331 :rpl_notopic)
(332 :rpl_topic)
(333 :rpl_topicwhotime) ; Seen in dancer ircd source
(341 :rpl_inviting)
(342 :rpl_summoning)
(346 :rpl_invitelist)
(347 :rpl_endofinvitelist)
(348 :rpl_exceptlist)
(349 :rpl_endofexceptlist)
(351 :rpl_version)
(352 :rpl_whoreply)
(353 :rpl_namreply)
(361 :rpl_killdone)
(362 :rpl_closing)
(363 :rpl_closeend)
(366 :rpl_endofnames)
(364 :rpl_links)
(365 :rpl_endoflinks)
(367 :rpl_banlist)
(368 :rpl_endofbanlist)
(369 :rpl_endofwhowas)
(371 :rpl_info)
(372 :rpl_motd)
(373 :rpl_infostart)
(374 :rpl_endofinfo)
(375 :rpl_motdstart)
(376 :rpl_endofmotd)
(377 :rpl_map) ; Seen in dancer ircd source
(378 :rpl_endofmap) ; Seen in dancer ircd source
(379 :rpl_forward) ; Seen in dancer ircd source
(381 :rpl_youreoper)
(382 :rpl_rehashing)
(383 :rpl_yourservice)
(384 :rpl_myportis)
(391 :rpl_time)
(392 :rpl_usersstart)
(393 :rpl_users)
(394 :rpl_endofusers)
(395 :rpl_nousers)
(399 :rpl_message) ; Seen in dancer ircd source
(401 :err_nosuchnick)
(402 :err_nosuchserver)
(403 :err_nosuchchannel)
(404 :err_cannotsendtochan)
(405 :err_toomanychannels)
(406 :err_wasnosuchnick)
(407 :err_toomanytargets)
(408 :err_nosuchservice)
(409 :err_noorigin)
(410 :err_services-offline) ; Seen in dancer ircd source
(411 :err_norecipient)
(412 :err_notexttosend)
(413 :err_notoplevel)
(414 :err_wildtoplevel)
(415 :err_badmask)
(421 :err_unknowncommand)
(422 :err_nomotd)
(423 :err_noadmininfo)
(424 :err_fileerror)
(431 :err_nonicknamegiven)
(432 :err_erroneusnickname)
(433 :err_nicknameinuse)
(436 :err_nickcollision)
(437 :err_unavailresource)
(438 :err_bannickchange) ; Seen in dancer ircd source
(441 :err_usernotinchannel)
(442 :err_notonchannel)
(443 :err_useronchannel)
(444 :err_nologin)
(445 :err_summondisabled)
(446 :err_userdisabled)
(447 :err_targetninvite) ; Seen in dancer ircd source
(448 :err_sourceninvite) ; Seen in dancer ircd source
(451 :err_notregistered)
(461 :err_needmoreparams)
(462 :err_alreadyregistered)
(463 :err_nopermforhost)
(464 :err_passwdmismatch)
(465 :err_yourebannedcreep)
(466 :err_youwillbebanned)
(467 :err_keyset)
(471 :err_channelisfull)
(472 :err_unknownmode)
(473 :err_inviteonlychan)
(474 :err_bannedfromchan)
(475 :err_badchannelkey)
(476 :err_badchanmask)
(477 :err_nochanmodes)
(478 :err_banlistfull)
(479 :err_badchanname) ; Seen in dancer ircd source
(480 :err_throttled) ; Seen in dancer ircd source
(481 :err_noprivileges)
(482 :err_chanoprivsneeded)
(483 :err_cantkillserver)
(484 :err_restricted)
(485 :err_uniqopprivsneeded)
(486 :err_restricted) ; Seen in dancer ircd source
(487 :err_no-op-split) ; Seen in dancer ircd source
(488 :err_need-umode) ; Seen in dancer ircd source
(491 :err_nooperhost)
(501 :err_umodeunknownflag)
(502 :err_usersdontmatch)
(503 :err_ghostedclient) ; Seen in dancer ircd source
(505 :err_blocking-notid) ; Seen in dancer ircd source
(511 :err_sitelistfull) ; Seen in dancer ircd source
(512 :err_maxmapnodes) ; Seen in dancer ircd source
(513 :err_maxforwarding) ; Seen in dancer ircd source
(514 :err_noforwarding) ; Seen in dancer ircd source
(515 :err_nounidentified) ; Seen in dancer ircd source
(516 :err_last_err_msg)
|#
