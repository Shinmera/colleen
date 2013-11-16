#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defgeneric reply->keyword (code)
  (:documentation "Take a reply code and turn it into a corresponding keyword if possible."))

(defmethod reply->keyword ((code integer))
  (or (gethash code *reply-code-map*)
      (intern (format NIL "~a" code) "KEYWORD")))

(defmethod reply->keyword ((code string))
  (handler-case (reply->keyword (parse-integer code))
    (error () (intern (string-upcase code) "KEYWORD"))))

(defparameter *reply-code-map*
  (let ((hash-table (make-hash-table :size 207)))
    (prog1 hash-table
      (mapc #'(lambda (reply) (setf (gethash (first reply) hash-table) (second reply)))
            ;; this list borrowed from cl-irc
            '((1 :rpl_welcome)
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
              (380 :rpl_banlinked) ; AIRCD
              (381 :rpl_youreoper)
              (382 :rpl_rehashing)
              (383 :rpl_yourservice)
              (384 :rpl_myportis)
              (385 :rpl_notoperanymore) ; AustHex, Hybrid, Unreal
              (386 :rpl_qlist) ; Unreal
              (387 :rpl_endofqlist) ; Unreal
              (388 :rpl_alist) ; Unreal
              (389 :rpl_endofalist) ; Unreal
              (391 :rpl_time)
              (392 :rpl_usersstart)
              (393 :rpl_users)
              (394 :rpl_endofusers)
              (395 :rpl_nousers)
              (396 :rpl_hosthidden) ; Undernet
              (399 :rpl_message) ; Seen in dancer ircd source
              (400 :err_unknownerror) 
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
              (516 :err_last_err_msg))))))
