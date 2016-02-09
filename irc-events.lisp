#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.events)

;;;;;;;;;;;;;;;; COMMANDS ;;;;;;;;;;;;;;;;

(define-event ping-event :PING (event) 
    (server1 &optional server2)
    (:documentation "Event on a PING request."))

(define-event pong-event :PONG (event) 
    (server1 &optional server2)
    (:documentation "Event on a PONG request."))

(define-event nick-event :NICK (user-event)
    (new-nick)
    (:documentation "Event on nick change."))

(define-event privmsg-event :PRIVMSG (channel-event)
    (channel message)
    (:documentation "Privmsg event."))

(defmethod initialize-instance :after ((event privmsg-event) &rest params)
  (declare (ignore params))
  (with-slots ((channel %channel) (message %message)) event
    (setf channel (first (arguments event))
          message (second (arguments event)))
    (unless (char= (aref channel 0) #\#)
      (setf channel (nick event)))))

(define-event join-event :JOIN (channel-event)
    ()
    (:documentation "User join event."))

(define-event part-event :PART (channel-event)
    ()
    (:documentation "User part event."))

(define-event quit-event :QUIT (user-event)
    (reason)
    (:documentation "User quit event."))

(define-event mode-event :MODE (user-event)
    (target mode &optional parameter)
    (:documentation "Mode change event."))

(define-event topic-set-event :TOPIC (channel-event)
    (channel topic)
    (:documentation "Topic set event."))

(define-event kick-event :KICK (channel-event)
    (channel target reason)
    (:documentation "User kick event."))

(define-event notice-event :NOTICE (user-event)
    (NIL message)
    (:documentation "Notice message."))

;;;;;;;;;;;;;;;; REPLIES ;;;;;;;;;;;;;;;;

;; Using the following webpage as a template, the following functions can
;; generate event definitions.
;; https://www.alien.net.au/irc/irc2numerics.html
;; Generated events need to be manually checked and adapted for various name
;; clashes though. These functions require CL-PPCRE and LQUERY to work.

;; (defun print-generated-event-definition (name origin format comment)
;;   (setf name (string-trim '(#\Newline #\Linefeed #\Space) name))
;;   (setf origin (string-trim '(#\Newline #\Linefeed #\Space) origin))
;;   (setf format (string-trim '(#\Newline #\Linefeed #\Space) format))
;;   (setf comment (string-trim '(#\Newline #\Linefeed #\Space) comment))
;;   (let ((group-name (cl-ppcre:regex-replace-all "_" (string-downcase (subseq name 4)) "-"))
;;         (format (CL-PPCRE:REGEX-REPLACE-ALL "[\\n]" (cl-ppcre:regex-replace-all "_" (cl-ppcre:regex-replace-all "[<>:]" (string-downcase format) "") "-") " "))
;;         (comment (CL-PPCRE:REGEX-REPLACE-ALL "[ ]{2}" (CL-PPCRE:REGEX-REPLACE-ALL "[\\n]" comment " ") " ")))
;;     (when (> (length format) 1)
;;       (format T "(define-event ~a-event :~a (event)~:[ ()~;~%    (~a)~]~%    (:documentation \"~a\"))~%~%" class-name name (> (length format) 1) format comment))))

;; (defun generate-event-definitions (path)
;;   ($ (initialize path :type :HTML) "tbody tr" 
;;      (each #'(lambda (a) 
;;                (print-generated-event-definition 
;;                 ($ a "td" (eq 1) (message) (node)) 
;;                 ($ a "td" (eq 2) (message) (node)) 
;;                 ($ a "td" (eq 3) (message) (node)) 
;;                 ($ a "td" (eq 4) (message) (node))) T)))
;;   NIL)


;;
(define-event welcome-event :RPL_WELCOME (event)
    (target message)
    (:documentation "The first message sent after client registration. The text used varies widely"))

(define-event your-host-event :RPL_YOURHOST (event)
    (target message)
    (:documentation "Part of the post-registration greeting. Text varies widely"))

(define-event created-event :RPL_CREATED (event)
    (target message)
    (:documentation "Part of the post-registration greeting. Text varies widely"))

(define-event myinfo-event :RPL_MYINFO (event)
    (target server-name &optional version user-modes chan-modes channel-modes-with-params user-modes-with-params server-modes server-modes-with-params)
    (:documentation "Same as RFC2812 however with additional fields to avoid additional 005 burden."))

(define-event bounce-event :RPL_BOUNCE (event)
    (target message)
    (:documentation "Sent by the server to a user to suggest an alternative server, sometimes used when the connection is refused because the server is already full. Also known as RPL_SLINE (AustHex), and RPL_REDIR Also see #010 ."))

(define-event isupport-event :RPL_ISUPPORT (event)
    (target &rest params)
    (:documentation "Also known as RPL_PROTOCTL (Bahamut, Unreal, Ultimate)"))

(define-event your-id-event :RPL_YOURID (event)
    (target &rest params)
    (:documentation ""))

(define-event save-nick-event :RPL_SAVENICK (event)
    (target message)
    (:documentation "Sent to the client when their nickname was forced to change due to a collision"))

(define-event trace-link-event :RPL_TRACELINK (event)
    (target link version destination next-server &optional vprotocol-version link-uptime-in-seconds backstream-sendq upstream-sendq)
    (:documentation "See RFC"))

(define-event trace-connecting-event :RPL_TRACECONNECTING (event)
    (target NIL group server-name)
    (:documentation "See RFC"))

(define-event trace-handshake-event :RPL_TRACEHANDSHAKE (event)
    (target NIL group server-name)
    (:documentation "See RFC"))

(define-event trace-unknown-event :RPL_TRACEUNKNOWN (event)
    (target NIL group &optional connection-address)
    (:documentation "See RFC"))

(define-event trace-operator-event :RPL_TRACEOPERATOR (event)
    (target oper group nick)
    (:documentation "See RFC"))

(define-event trace-user-event :RPL_TRACEUSER (event)
    (target user group nick)
    (:documentation "See RFC"))

(define-event trace-server-event :RPL_TRACESERVER (event)
    (target serv group ints intc server-name hostname &optional vprotocol-version)
    (:documentation "See RFC"))

(define-event trace-service-event :RPL_TRACESERVICE (event)
    (target service group name service-type active-type)
    (:documentation "See RFC"))

(define-event trace-newtype-event :RPL_TRACENEWTYPE (event)
    (target newtype NIL client-name)
    (:documentation "See RFC"))

(define-event trace-class-event :RPL_TRACECLASS (event)
    (target NIL group class-count)
    (:documentation "See RFC"))

(define-event stats-linkinfo-event :RPL_STATSLINKINFO (event)
    (target linkname sendq sent-msgs sent-bytes recvd-msgs rcvd-bytes time-open)
    (:documentation "Reply to STATS (See RFC)"))

(define-event stats-commands-event :RPL_STATSCOMMANDS (event)
    (target command cmd-count &optional byte-count remote-count)
    (:documentation "Reply to STATS (See RFC)"))

(define-event stats-cline-event :RPL_STATSCLINE (event)
    (target NIL host NIL name port group)
    (:documentation "Reply to STATS (See RFC)"))

(define-event stats-nline-event :RPL_STATSNLINE (event)
    (target NIL host NIL name port group)
    (:documentation "Reply to STATS (See RFC), Also known as RPL_STATSOLDNLINE (ircu, Unreal)"))

(define-event stats-iline-event :RPL_STATSILINE (event)
    (target NIL host NIL host2 port group)
    (:documentation "Reply to STATS (See RFC)"))

(define-event stats-kline-event :RPL_STATSKLINE (event)
    (target NIL host NIL username port group)
    (:documentation "Reply to STATS (See RFC)"))

(define-event stats-yline-event :RPL_STATSYLINE (event)
    (target NIL group ping-freq connect-freq max-sendq)
    (:documentation "Reply to STATS (See RFC)"))

(define-event stats-end-event :RPL_ENDOFSTATS (event)
    (target query message)
    (:documentation "End of RPL_STATS* list."))

(define-event umode-event :RPL_UMODEIS (event)
    (target user-modes &optional user-mode-params)
    (:documentation "Information about a user's own modes. Some daemons have extended the mode command and certain modes take parameters (like channel modes)."))

(define-event servlist-event :RPL_SERVLIST (event)
    (target name server-name mask server-type hopcount message)
    (:documentation "A service entry in the service list"))

(define-event servlist-end-event :RPL_SERVLISTEND (event)
    (target mask server-type message)
    (:documentation "Termination of an RPL_SERVLIST list"))

(define-event statsl-line-event :RPL_STATSLLINE (event)
    (target NIL hostmask NIL server-name maxdepth)
    (:documentation "Reply to STATS (See RFC)"))

(define-event stats-uptime-event :RPL_STATSUPTIME (event)
    (target message)
    (:documentation "Reply to STATS (See RFC)"))

(define-event stats-oline-event :RPL_STATSOLINE (event)
    (target NIL hostmask NIL nick &optional message)
    (:documentation "Reply to STATS (See RFC); The info field is an extension found in some IRC daemons, which returns info such as an e-mail address or the name/job of an operator"))

(define-event stats-hline-event :RPL_STATSHLINE (event)
    (target NIL hostmask NIL server-name)
    (:documentation "Reply to STATS (See RFC)"))

(define-event luser-client-event :RPL_LUSERCLIENT (event)
    (target message)
    (:documentation "Reply to LUSERS command, other versions exist (eg. RFC2812); Text may vary."))

(define-event luser-op-event :RPL_LUSEROP (event)
    (target int message)
    (:documentation "Reply to LUSERS command - Number of IRC operators online"))

(define-event luser-unknown-event :RPL_LUSERUNKNOWN (event)
    (target int message)
    (:documentation "Reply to LUSERS command - Number of unknown/unregistered connections"))

(define-event luser-channels-event :RPL_LUSERCHANNELS (event)
    (target int message)
    (:documentation "Reply to LUSERS command - Number of channels formed"))

(define-event luser-me-event :RPL_LUSERME (event)
    (target message)
    (:documentation "Reply to LUSERS command - Information about local connections; Text may vary."))

(define-event admin-me-event :RPL_ADMINME (event)
    (target server-name message)
    (:documentation "Start of an RPL_ADMIN* reply. In practise, the server parameter is often never given, and instead the info field contains the text 'Administrative info about <server>'. Newer daemons seem to follow the RFC and output the server's hostname in the 'server' parameter, but also output the server name in the text as per traditional daemons."))

(define-event admin-loc1-event :RPL_ADMINLOC1 (event)
    (target admin-location)
    (:documentation "Reply to ADMIN command (Location, first line)"))

(define-event admin-loc2-event :RPL_ADMINLOC2 (event)
    (target admin-location)
    (:documentation "Reply to ADMIN command (Location, second line)"))

(define-event admin-email-event :RPL_ADMINEMAIL (event)
    (target email-address)
    (:documentation "Reply to ADMIN command (E-mail address of administrator)"))

(define-event trace-log-event :RPL_TRACELOG (event)
    (target file logfile debug-level)
    (:documentation "See RFC"))

(define-event trace-end-event :RPL_TRACEEND (event)
    (target server-name version message)
    (:documentation "Used to terminate a list of RPL_TRACE* replies"))

(define-event try-again-event :RPL_TRYAGAIN (event)
    (target command message)
    (:documentation "When a server drops a command without processing it, it MUST use this reply. Also known as RPL_LOAD_THROTTLED and RPL_LOAD2HI, I'm presuming they do the same thing."))

(define-event local-users-event :RPL_LOCALUSERS (event)
    (target &rest params)
    (:documentation "Also known as RPL_CURRENT_LOCAL"))

(define-event global-users-event :RPL_GLOBALUSERS (event)
    (target &rest params)
    (:documentation "Also known as RPL_CURRENT_GLOBAL"))

(define-event away-event :RPL_AWAY (event)
    (target nick message)
    (:documentation "Used in reply to a command directed at a user who is marked as away"))

(define-event userhost-event :RPL_USERHOST (event)
    (target message)
    (:documentation "Reply used by USERHOST (see RFC)"))

(define-event ison-event :RPL_ISON (event)
    (target message)
    (:documentation "Reply to the ISON command (see RFC)"))

(define-event un-away-event :RPL_UNAWAY (event)
    (target message)
    (:documentation "Reply from AWAY when no longer marked as away"))

(define-event now-away-event :RPL_NOWAWAY (event)
    (target message)
    (:documentation "Reply from AWAY when marked away"))

(define-event whois-user-event :RPL_WHOISUSER (event)
    (target nick user host NIL real-name)
    (:documentation "Reply to WHOIS - Information about the user"))

(define-event whois-server-event :RPL_WHOISSERVER (event)
    (target nick server-name server-info)
    (:documentation "Reply to WHOIS - What server they're on"))

(define-event whois-operator-event :RPL_WHOISOPERATOR (event)
    (target nick privileges)
    (:documentation "Reply to WHOIS - User has IRC Operator privileges"))

(define-event whowas-user-event :RPL_WHOWASUSER (event)
    (target nick user host NIL real-name)
    (:documentation "Reply to WHOWAS - Information about the user"))

(define-event who-end-event :RPL_ENDOFWHO (event)
    (target name message)
    (:documentation "Used to terminate a list of RPL_WHOREPLY replies"))

(define-event whois-idle-event :RPL_WHOISIDLE (event)
    (target nick seconds message)
    (:documentation "Reply to WHOIS - Idle information"))

(define-event whois-end-event :RPL_ENDOFWHOIS (event)
    (target nick message)
    (:documentation "Reply to WHOIS - End of list"))

(define-event whois-channels-event :RPL_WHOISCHANNELS (event)
    (target nick message)
    (:documentation "Reply to WHOIS - Channel list for user (See RFC)"))

(define-event list-start-event :RPL_LISTSTART (event)
    (target channels message)
    (:documentation "Channel list - Header"))

(define-event list-event :RPL_LIST (event)
    (target channel visible topic)
    (:documentation "Channel list - A channel"))

(define-event list-end-event :RPL_LISTEND (event)
    (target message)
    (:documentation "Channel list - End of list"))

(define-event channel-mode-event :RPL_CHANNELMODEIS (event)
    (target channel mode mode-params)
    (:documentation ""))

(define-event uniqop-event :RPL_UNIQOPIS (event)
    (target channel nickname)
    (:documentation ""))

(define-event whois-account-event :RPL_WHOISACCOUNT (event)
    (target nick authname message)
    (:documentation ""))

(define-event no-topic-event :RPL_NOTOPIC (event)
    (target channel message)
    (:documentation "Response to TOPIC when no topic is set"))

(define-event topic-event :RPL_TOPIC (event)
    (target channel topic)
    (:documentation "Response to TOPIC with the set topic"))

(define-event topic-who-time-event :RPL_TOPICWHOTIME (event)
    (target &rest params)
    (:documentation ""))

(define-event inviting-event :RPL_INVITING (event)
    (target nick channel)
    (:documentation "Returned by the server to indicate that the attempted INVITE message was successful and is being passed onto the end client. Note that RFC1459 documents the parameters in the reverse order. The format given here is the format used on production servers, and should be considered the standard reply above that given by RFC1459."))

(define-event summoning-event :RPL_SUMMONING (event)
    (target user message)
    (:documentation "Returned by a server answering a SUMMON message to indicate that it is summoning that user"))

(define-event invited-event :RPL_INVITED (event)
    (target channel invitee sender message)
    (:documentation "Sent to users on a channel when an INVITE command has been issued"))

(define-event invitelist-event :RPL_INVITELIST (event)
    (target channel invitemask)
    (:documentation "An invite mask for the invite mask list"))

(define-event invitelist-end-event :RPL_ENDOFINVITELIST (event)
    (target channel message)
    (:documentation "Termination of an RPL_INVITELIST list"))

(define-event exceptlist-event :RPL_EXCEPTLIST (event)
    (target channel exceptionmask)
    (:documentation "An exception mask for the exception mask list. Also known as RPL_EXLIST (Unreal, Ultimate)"))

(define-event exceptlist-end-event :RPL_ENDOFEXCEPTLIST (event)
    (target channel message)
    (:documentation "Termination of an RPL_EXCEPTLIST list. Also known as RPL_ENDOFEXLIST (Unreal, Ultimate)"))

(define-event version-event :RPL_VERSION (event)
    (target version server-name comments)
    (:documentation "Reply by the server showing its version details, however this format is not often adhered to"))

(define-event who-reply-event :RPL_WHOREPLY (event)
    (target channel user host server-name nick &rest params)
    (:documentation "Reply to vanilla WHO (See RFC). This format can be very different if the 'WHOX' version of the command is used (see ircu)."))

(define-event namreply-event :RPL_NAMREPLY (event)
    (target name-type channel nickinfo)
    (:documentation "Reply to NAMES (See RFC)"))

;; KLUDGE!
;; IRC reply messages include a way to specify a "rest" argument.
;; This argument is separated from the rest by a colon. The rest
;; argument can contain arbitrary data, from a message string to
;; anything else, even useful things such as, in this case, a nick
;; list. In order to accommodate for the general case, Colleen
;; bunches the "rest" argument into a whole string, meaning we
;; can't simply use the &rest clause in our definition above.
;;
;; Even though our definition system contains an extra &string
;; clause that would make this duality trivial to handle, it would
;; also require me going through all of the events in this file
;; manually and seeing which ones make use of the "rest" argument
;; when it should be kept a string and which ones require a list.
;; Since this file is big and the change could break other things
;; I'm not going to bother with it right now. However, this is
;; definitely something that should be fixed in the future.
(defmethod initialize-instance :after ((event namreply-event) &key)
  (colleen::arguments-bind (target name-type channel nickinfo) (arguments event)
    (setf (slot-value event '%target) target)
    (setf (slot-value event '%name-type) name-type)
    (setf (slot-value event '%channel) channel)
    (setf (slot-value event '%nickinfo) (cl-ppcre:split "\\s+" nickinfo))))

(define-event links-event :RPL_LINKS (event)
    (target mask server-name hopcount server-info)
    (:documentation "Reply to the LINKS command"))

(define-event links-end-event :RPL_ENDOFLINKS (event)
    (target mask message)
    (:documentation "Termination of an RPL_LINKS list"))

(define-event names-end-event :RPL_ENDOFNAMES (event)
    (target channel message)
    (:documentation "Termination of an RPL_NAMREPLY list"))

(define-event banlist-event :RPL_BANLIST (event)
    (target channel banid &optional time-left reason )
    (:documentation "A ban-list item (See RFC); <time left> and <reason> are additions used by KineIRCd"))

(define-event banlist-end-event :RPL_ENDOFBANLIST (event)
    (target channel message)
    (:documentation "Termination of an RPL_BANLIST list"))

(define-event whowas-end-event :RPL_ENDOFWHOWAS (event)
    (target nick message)
    (:documentation "Reply to WHOWAS - End of list"))

(define-event info-event :RPL_INFO (event)
    (target message)
    (:documentation "Reply to INFO"))

(define-event motd-event :RPL_MOTD (event)
    (target message)
    (:documentation "Reply to MOTD"))

(define-event info-end-event :RPL_ENDOFINFO (event)
    (target message)
    (:documentation "Termination of an RPL_INFO list"))

(define-event motd-start-event :RPL_MOTDSTART (event)
    (target message)
    (:documentation "Start of an RPL_MOTD list"))

(define-event motd-end-event :RPL_ENDOFMOTD (event)
    (target message)
    (:documentation "Termination of an RPL_MOTD list"))

(define-event spam-event :RPL_SPAM (event)
    (target message)
    (:documentation "Used during the connection (after MOTD) to announce the network policy on spam and privacy. Supposedly now obsoleted in favour of using NOTICE."))

(define-event youre-oper-event :RPL_YOUREOPER (event)
    (target message)
    (:documentation "Successful reply from OPER"))

(define-event rehashing-event :RPL_REHASHING (event)
    (target config-file message)
    (:documentation "Successful reply from REHASH"))

(define-event youre-service-event :RPL_YOURESERVICE (event)
    (target message)
    (:documentation "Sent upon successful registration of a service"))

(define-event time-event :RPL_TIME (event)
    (target server-name time-string &rest params)
    (:documentation "Response to the TIME command. The string format may vary greatly. Also see #679 ."))

(define-event users-start-event :RPL_USERSSTART (event)
    (target message)
    (:documentation "Start of an RPL_USERS list"))

(define-event users-event :RPL_USERS (event)
    (target message)
    (:documentation "Response to the USERS command (See RFC)"))

(define-event users-end-event :RPL_ENDOFUSERS (event)
    (target message)
    (:documentation "Termination of an RPL_USERS list"))

(define-event nousers-event :RPL_NOUSERS (event)
    (target message)
    (:documentation "Reply to USERS when nobody is logged in"))

(define-event host-hidden-event :RPL_HOSTHIDDEN (event)
    (target &rest params)
    (:documentation "Reply to a user when user mode +x (host masking) was set successfully"))

(define-event unknow-nerror-event :ERR_UNKNOWNERROR (event)
    (target command &string message)
    (:documentation "Sent when an error occured executing a command, but it is not specifically known why the command could not be executed."))

(define-event nosuch-nick-event :ERR_NOSUCHNICK (event)
    (target nick reason)
    (:documentation "Used to indicate the nickname parameter supplied to a command is currently unused"))

(define-event nosuch-server-event :ERR_NOSUCHSERVER (event)
    (target server-name reason)
    (:documentation "Used to indicate the server name given currently doesn't exist"))

(define-event nosuch-channel-event :ERR_NOSUCHCHANNEL (event)
    (target channel reason)
    (:documentation "Used to indicate the given channel name is invalid, or does not exist"))

(define-event cannot-send-to-chan-event :ERR_CANNOTSENDTOCHAN (event)
    (target channel reason)
    (:documentation "Sent to a user who does not have the rights to send a message to a channel"))

(define-event too-many-channels-event :ERR_TOOMANYCHANNELS (event)
    (target channel reason)
    (:documentation "Sent to a user when they have joined the maximum number of allowed channels and they tried to join another channel"))

(define-event was-no-such-nick-event :ERR_WASNOSUCHNICK (event)
    (target nick reason)
    (:documentation "Returned by WHOWAS to indicate there was no history information for a given nickname"))

(define-event too-many-targets-event :ERR_TOOMANYTARGETS (event)
    (target targets reason)
    (:documentation "The given target(s) for a command are ambiguous in that they relate to too many targets"))

(define-event no-such-service-event :ERR_NOSUCHSERVICE (event)
    (target service-name reason)
    (:documentation "Returned to a client which is attempting to send an SQUERY (or other message) to a service which does not exist"))

(define-event no-origin-event :ERR_NOORIGIN (event)
    (target reason)
    (:documentation "PING or PONG message missing the originator parameter which is required since these commands must work without valid prefixes"))

(define-event no-recipient-event :ERR_NORECIPIENT (event)
    (target reason)
    (:documentation "Returned when no recipient is given with a command"))

(define-event no-text-to-send-event :ERR_NOTEXTTOSEND (event)
    (target reason)
    (:documentation "Returned when NOTICE/PRIVMSG is used with no message given"))

(define-event no-top-level-event :ERR_NOTOPLEVEL (event)
    (target mask reason)
    (:documentation "Used when a message is being sent to a mask without being limited to a top-level domain (i.e. * instead of *.au)"))

(define-event wild-top-level-event :ERR_WILDTOPLEVEL (event)
    (target mask reason)
    (:documentation "Used when a message is being sent to a mask with a wild-card for a top level domain (i.e. *.*)"))

(define-event bad-mask-event :ERR_BADMASK (event)
    (target mask reason)
    (:documentation "Used when a message is being sent to a mask with an invalid syntax"))

(define-event too-many-matches-event :ERR_TOOMANYMATCHES (event)
    (target command &optional mask message)
    (:documentation "Returned when too many matches have been found for a command and the output has been truncated. An example would be the WHO command, where by the mask '*' would match everyone on the network! Ouch!"))

(define-event unknown-command-event :ERR_UNKNOWNCOMMAND (event)
    (target command reason)
    (:documentation "Returned when the given command is unknown to the server (or hidden because of lack of access rights)"))

(define-event no-motd-event :ERR_NOMOTD (event)
    (target reason)
    (:documentation "Sent when there is no MOTD to send the client"))

(define-event no-admininfo-event :ERR_NOADMININFO (event)
    (target server-name reason)
    (:documentation "Returned by a server in response to an ADMIN request when no information is available. RFC1459 mentions this in the list of numerics. While it's not listed as a valid reply in section 4.3.7 ('Admin command'), it's confirmed to exist in the real world."))

(define-event file-error-event :ERR_FILEERROR (event)
    (target reason)
    (:documentation "Generic error message used to report a failed file operation during the processing of a command"))

(define-event no-nickname-given-event :ERR_NONICKNAMEGIVEN (event)
    (target reason)
    (:documentation "Returned when a nickname parameter expected for a command isn't found"))

(define-event erroneusnickname-event :ERR_ERRONEUSNICKNAME (event)
    (target nick reason)
    (:documentation "Returned after receiving a NICK message which contains a nickname which is considered invalid, such as it's reserved ('anonymous') or contains characters considered invalid for nicknames. This numeric is misspelt, but remains with this name for historical reasons :)"))

(define-event nickname-in-use-event :ERR_NICKNAMEINUSE (event)
    (target nick reason)
    (:documentation "Returned by the NICK command when the given nickname is already in use"))

(define-event nick-collision-event :ERR_NICKCOLLISION (event)
    (target nick reason)
    (:documentation "Returned by a server to a client when it detects a nickname collision"))

(define-event unavail-resource-event :ERR_UNAVAILRESOURCE (event)
    (target resource reason)
    (:documentation "Return when the target is unable to be reached temporarily, eg. a delay mechanism in play, or a service being offline"))

(define-event user-not-in-channel-event :ERR_USERNOTINCHANNEL (event)
    (target nick channel reason)
    (:documentation "Returned by the server to indicate that the target user of the command is not on the given channel"))

(define-event not-on-channel-event :ERR_NOTONCHANNEL (event)
    (target channel reason)
    (:documentation "Returned by the server whenever a client tries to perform a channel effecting command for which the client is not a member"))

(define-event user-on-channel-event :ERR_USERONCHANNEL (event)
    (target nick channel &string reason)
    (:documentation "Returned when a client tries to invite a user to a channel they're already on"))

(define-event no-login-event :ERR_NOLOGIN (event)
    (target user reason)
    (:documentation "Returned by the SUMMON command if a given user was not logged in and could not be summoned"))

(define-event summon-disabled-event :ERR_SUMMONDISABLED (event)
    (target reason)
    (:documentation "Returned by SUMMON when it has been disabled or not implemented"))

(define-event users-disabled-event :ERR_USERSDISABLED (event)
    (target reason)
    (:documentation "Returned by USERS when it has been disabled or not implemented"))

(define-event not-implemented-event :ERR_NOTIMPLEMENTED (event)
    (target &rest params)
    (:documentation "Returned when a requested feature is not implemented (and cannot be completed)"))

(define-event not-registered-event :ERR_NOTREGISTERED (event)
    (target reason)
    (:documentation "Returned by the server to indicate that the client must be registered before the server will allow it to be parsed in detail"))

(define-event need-more-params-event :ERR_NEEDMOREPARAMS (event)
    (target command reason)
    (:documentation "Returned by the server by any command which requires more parameters than the number of parameters given"))

(define-event already-registered-event :ERR_ALREADYREGISTERED (event)
    (target reason)
    (:documentation "Returned by the server to any link which attempts to register again"))

(define-event no-perm-for-host-event :ERR_NOPERMFORHOST (event)
    (target reason)
    (:documentation "Returned to a client which attempts to register with a server which has been configured to refuse connections from the client's host"))

(define-event password-mismatch-event :ERR_PASSWDMISMATCH (event)
    (target reason)
    (:documentation "Returned by the PASS command to indicate the given password was required and was either not given or was incorrect"))

(define-event youre-banned-creep-event :ERR_YOUREBANNEDCREEP (event)
    (target reason)
    (:documentation "Returned to a client after an attempt to register on a server configured to ban connections from that client"))

(define-event key-set-event :ERR_KEYSET (event)
    (target channel reason)
    (:documentation "Returned when the channel key for a channel has already been set"))

(define-event channel-full-event :ERR_CHANNELISFULL (event)
    (target channel reason)
    (:documentation "Returned when attempting to join a channel which is set +l and is already full"))

(define-event unknown-mode-event :ERR_UNKNOWNMODE (event)
    (target mode-char reason)
    (:documentation "Returned when a given mode is unknown"))

(define-event inviteonly-chan-event :ERR_INVITEONLYCHAN (event)
    (target channel reason)
    (:documentation "Returned when attempting to join a channel which is invite only without an invitation"))

(define-event banned-from-chan-event :ERR_BANNEDFROMCHAN (event)
    (target channel reason)
    (:documentation "Returned when attempting to join a channel a user is banned from"))

(define-event bad-channel-key-event :ERR_BADCHANNELKEY (event)
    (target channel reason)
    (:documentation "Returned when attempting to join a key-locked channel either without a key or with the wrong key"))

(define-event bad-channel-mask-event :ERR_BADCHANMASK (event)
    (target channel reason)
    (:documentation "The given channel mask was invalid"))

(define-event no-channel-modes-event :ERR_NOCHANMODES (event)
    (target channel reason)
    (:documentation "Returned when attempting to set a mode on a channel which does not support channel modes, or channel mode changes. Also known as ERR_MODELESS"))

(define-event banlist-full-event :ERR_BANLISTFULL (event)
    (target channel chan-char reason)
    (:documentation "Returned when a channel access list (i.e. ban list etc) is full and cannot be added to"))

(define-event no-privileges-event :ERR_NOPRIVILEGES (event)
    (target reason)
    (:documentation "Returned by any command requiring special privileges (eg. IRC operator) to indicate the operation was unsuccessful"))

(define-event chan-privs-needed-event :ERR_CHANOPRIVSNEEDED (event)
    (target channel reason)
    (:documentation "Returned by any command requiring special channel privileges (eg. channel operator) to indicate the operation was unsuccessful"))

(define-event cant-kill-server-event :ERR_CANTKILLSERVER (event)
    (target reason)
    (:documentation "Returned by KILL to anyone who tries to kill a server"))

(define-event restricted-event :ERR_RESTRICTED (event)
    (target reason)
    (:documentation "Sent by the server to a user upon connection to indicate the restricted nature of the connection (i.e. usermode +r)"))

(define-event unique-privs-needed-event :ERR_UNIQOPRIVSNEEDED (event)
    (target reason)
    (:documentation "Any mode requiring 'channel creator' privileges returns this error if the client is attempting to use it while not a channel creator on the given channel"))

(define-event no-operhost-event :ERR_NOOPERHOST (event)
    (target reason)
    (:documentation "Returned by OPER to a client who cannot become an IRC operator because the server has been configured to disallow the client's host"))

(define-event mode-unknown-flag-event :ERR_UMODEUNKNOWNFLAG (event)
    (target reason)
    (:documentation "Returned by the server to indicate that a MODE message was sent with a nickname parameter and that the mode flag sent was not recognised"))

(define-event users-dont-match-event :ERR_USERSDONTMATCH (event)
    (target reason)
    (:documentation "Error sent to any user trying to view or change the user mode for a user other than themselves"))

(define-event vworld-warn-event :ERR_VWORLDWARN (event)
    (target message)
    (:documentation "Warning about Virtual-World being turned off. Obsoleted in favour for RPL_MODECHANGEWARN Also see #662 ."))

(define-event disabled-event :ERR_DISABLED (event)
    (target command reason)
    (:documentation ""))

(define-event remote-pfx-event :ERR_REMOTEPFX (event)
    (target nickname reason)
    (:documentation "Proposed."))

(define-event pfx-unroutable-event :ERR_PFXUNROUTABLE (event)
    (target nickname reason)
    (:documentation "Proposed."))

(define-event traceroute-hop-event :RPL_TRACEROUTE_HOP (event)
    (target target-trace hop &optional address hostname usec-ping)
    (:documentation "Returned from the TRACEROUTE IRC-Op command when tracerouting a host"))

(define-event traceroute-start-event :RPL_TRACEROUTE_START (event)
    (target target-trace target-fqdn target-address max-hops)
    (:documentation "Start of an RPL_TRACEROUTE_HOP list"))

(define-event mode-change-warn-event :RPL_MODECHANGEWARN (event)
    (target mode message)
    (:documentation "Plain text warning to the user about turning on or off a user mode. If no '+' or '-' prefix is used for the mode char, '+' is presumed."))

(define-event chanredir-event :RPL_CHANREDIR (event)
    (target old-chan new-chan message)
    (:documentation "Used to notify the client upon JOIN that they are joining a different channel than expected because the IRC Daemon has been set up to map the channel they attempted to join to the channel they eventually will join."))

(define-event servmode-event :RPL_SERVMODEIS (event)
    (target server-name modes &rest parameters)
    (:documentation "Reply to MODE <servername>. KineIRCd supports server modes to simplify configuration of servers; Similar to RPL_CHANNELMODEIS"))

(define-event other-umode-event :RPL_OTHERUMODEIS (event)
    (target nickname modes)
    (:documentation "Reply to MODE <nickname> to return the user-modes of another user to help troubleshoot connections, etc. Similar to RPL_UMODEIS, however including the target"))

(define-event generic-end-event :RPL_ENDOF_GENERIC (event)
    (target command &rest parameter)
    (:documentation "Generic response for new lists to save numerics."))

(define-event whowas-details-event :RPL_WHOWASDETAILS (event)
    (target nick who-type message)
    (:documentation "Returned by WHOWAS to return extended information (if available). The type field is a number indication what kind of information."))

(define-event whois-secure-event :RPL_WHOISSECURE (event)
    (target nick who-type &optional message)
    (:documentation "Reply to WHOIS command - Returned if the target is connected securely, eg. type may be TLSv1, or SSLv2 etc. If the type is unknown, a '*' may be used."))

(define-event unknown-modes-event :RPL_UNKNOWNMODES (event)
    (target modes message)
    (:documentation "Returns a full list of modes that are unknown when a client issues a MODE command (rather than one numeric per mode)"))

(define-event cannot-set-modes-event :RPL_CANNOTSETMODES (event)
    (target modes message)
    (:documentation "Returns a full list of modes that cannot be set when a client issues a MODE command"))

(define-event luser-staff-event :RPL_LUSERSTAFF (event)
    (target staff-online-count message)
    (:documentation "Reply to LUSERS command - Number of network staff (or 'helpers') online (differs from Local/Global operators). Similar format to RPL_LUSEROP"))

(define-event time-on-server-event :RPL_TIMEONSERVERIS (event)
    (target seconds nanoseconds timezone flags message)
    (:documentation "Optionally sent upon connection, and/or sent as a reply to the TIME command. This returns the time on the server in a uniform manner. The seconds (and optionally nanoseconds) is the time since the UNIX Epoch, and is used since many existing timestamps in the IRC-2 protocol are done this way (i.e. ban lists). The timezone is hours and minutes each of Greenwich ('[+/-]HHMM'). Since all timestamps sent from the server are in a similar format, this numeric is designed to give clients the ability to provide accurate timestamps to their users."))

(define-event networks-event :RPL_NETWORKS (event)
    (target name through-name hops message)
    (:documentation "A reply to the NETWORKS command when requesting a list of known networks (within the IIRC domain)."))

(define-event your-language-event :RPL_YOURLANGUAGEIS (event)
    (target code message)
    (:documentation "Reply to the LANGUAGE command, informing the client of the language(s) it has set"))

(define-event language-event :RPL_LANGUAGE (event)
    (target code revision maintainer flags NIL message)
    (:documentation "A language reply to LANGUAGE when requesting a list of known languages"))

(define-event whois-staff-event :RPL_WHOISSTAFF (event)
    (target message)
    (:documentation "The user is a staff member. The information may explain the user's job role, or simply state that they are a part of the network staff. Staff members are not IRC operators, but rather people who have special access in association with network services. KineIRCd uses this numeric instead of the existing numerics due to the overwhelming number of conflicts."))

(define-event whois-language-event :RPL_WHOISLANGUAGE (event)
    (target nick language codes)
    (:documentation "Reply to WHOIS command - A list of languages someone can speak. The language codes are comma delimitered."))

(define-event modlist-event :RPL_MODLIST (event)
    (target &rest params)
    (:documentation "Output from the MODLIST command"))

(define-event modlist-end-event :RPL_ENDOFMODLIST (event)
    (target message)
    (:documentation "Terminates MODLIST output"))

(define-event help-start-event :RPL_HELPSTART (event)
    (target command message)
    (:documentation "Start of HELP command output"))

(define-event help-txt-event :RPL_HELPTXT (event)
    (target command message)
    (:documentation "Output from HELP command"))

(define-event help-end-event :RPL_ENDOFHELP (event)
    (target command message)
    (:documentation "End of HELP command output"))

(define-event etrace-full-event :RPL_ETRACEFULL (event)
    (target &rest params)
    (:documentation "Output from 'extended' trace"))

(define-event etrace-event :RPL_ETRACE (event)
    (target &rest params)
    (:documentation "Output from 'extended' trace"))

(define-event knock-event :RPL_KNOCK (event)
    (target channel user message)
    (:documentation "Message delivered using KNOCK command"))

(define-event knock-dlvr-event :RPL_KNOCKDLVR (event)
    (target channel message)
    (:documentation "Message returned from using KNOCK command"))

(define-event too-many-knock-event :ERR_TOOMANYKNOCK (event)
    (target channel message)
    (:documentation "Message returned when too many KNOCKs for a channel have been sent by a user"))

(define-event chan-open-event :ERR_CHANOPEN (event)
    (target channel message)
    (:documentation "Message returned from KNOCK when the channel can be freely joined by the user"))

(define-event knock-on-chan-event :ERR_KNOCKONCHAN (event)
    (target channel message)
    (:documentation "Message returned from KNOCK when the user has used KNOCK on a channel they have already joined"))

(define-event knock-disabled-event :ERR_KNOCKDISABLED (event)
    (target message)
    (:documentation "Returned from KNOCK when the command has been disabled"))

(define-event target-umodeg-event :RPL_TARGUMODEG (event)
    (target nick message)
    (:documentation "Sent to indicate the given target is set +g (server-side ignore)"))

(define-event target-notify-event :RPL_TARGNOTIFY (event)
    (target nick message)
    (:documentation "Sent following a PRIVMSG/NOTICE to indicate the target has been notified of an attempt to talk to them while they are set +g"))

(define-event umodeg-msg-event :RPL_UMODEGMSG (event)
    (target nick user message)
    (:documentation "Sent to a user who is +g to inform them that someone has attempted to talk to them (via PRIVMSG/NOTICE), and that they will need to be accepted (via the ACCEPT command) before being able to talk to them"))

(define-event omotd-start-event :RPL_OMOTDSTART (event)
    (target message)
    (:documentation "IRC Operator MOTD header, sent upon OPER command"))

(define-event omotd-event :RPL_OMOTD (event)
    (target message)
    (:documentation "IRC Operator MOTD text (repeated, usually)"))

(define-event omotd-end-event :RPL_ENDOFOMOTD (event)
    (target message)
    (:documentation "IRC operator MOTD footer"))

(define-event no-privs-event :ERR_NOPRIVS (event)
    (target command message)
    (:documentation "Returned from an oper command when the IRC operator does not have the relevant operator privileges."))

(define-event testmark-event :RPL_TESTMARK (event)
    (target user NIL NIL message)
    (:documentation "Reply from an oper command reporting how many users match a given user@host mask"))

(define-event testline-event :RPL_TESTLINE (event)
    (target &rest params)
    (:documentation "Reply from an oper command reporting relevant I/K lines that will match a given user@host"))

(define-event no-testline-event :RPL_NOTESTLINE (event)
    (target NIL message)
    (:documentation "Reply from oper command reporting no I/K lines match the given user@host"))

(define-event cannot-change-umode-event :ERR_CANNOTCHANGEUMODE (event)
    (target mode-char reason)
    (:documentation "Reply to MODE when a user cannot change a user mode"))

(define-event cannot-change-chanmode-event :ERR_CANNOTCHANGECHANMODE (event)
    (target mode-char reason)
    (:documentation "Reply to MODE when a user cannot change a channel mode"))

(define-event cannot-change-servermode-event :ERR_CANNOTCHANGESERVERMODE (event)
    (target mode-char reason)
    (:documentation "Reply to MODE when a user cannot change a server mode"))

(define-event cannot-send-to-nick-event :ERR_CANNOTSENDTONICK (event)
    (target nick reason)
    (:documentation "Returned from NOTICE, PRIVMSG or other commands to notify the user that they cannot send a message to a particular client. Similar to ERR_CANNOTSENDTOCHAN. KineIRCd uses this in conjunction with user-mode +R to allow users to block people who are not identified to services (spam avoidance)"))

(define-event unknown-server-mode-event :ERR_UNKNOWNSERVERMODE (event)
    (target modechar message)
    (:documentation "Returned by MODE to inform the client they used an unknown server mode character."))

(define-event server-mode-lock-event :ERR_SERVERMODELOCK (event)
    (target message)
    (:documentation "Returned by MODE to inform the client the server has been set mode +L by an administrator to stop server modes being changed"))

(define-event bad-char-encoding-event :ERR_BADCHARENCODING (event)
    (target command charset message)
    (:documentation "Returned by any command which may have had the given data modified because one or more glyphs were incorrectly encoded in the current charset (given). Such a use would be where an invalid UTF-8 sequence was given which may be considered insecure, or defines a character which is invalid within that context. For safety reasons, the invalid character is not returned to the client."))

(define-event too-many-languages-event :ERR_TOOMANYLANGUAGES (event)
    (target max-langs message)
    (:documentation "Returned by the LANGUAGE command to tell the client they cannot set as many languages as they have requested. To assist the client, the maximum languages which can be set at one time is given, and the language settings are not changed."))

(define-event no-language-event :ERR_NOLANGUAGE (event)
    (target language-code message)
    (:documentation "Returned by the LANGUAGE command to tell the client it has specified an unknown language code."))

(define-event text-too-short-event :ERR_TEXTTOOSHORT (event)
    (target command message)
    (:documentation "Returned by any command requiring text (such as a message or a reason), which was not long enough to be considered valid. This was created initially to combat '/wallops foo' abuse, but is also used by DIE and RESTART commands to attempt to encourage meaningful reasons."))
