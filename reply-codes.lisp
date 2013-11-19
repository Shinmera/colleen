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
            ;; this list is generated from https://www.alien.net.au/irc/irc2numerics.html
            ;; Some status codes are mapped to multiple names.
            '((1 :RPL_WELCOME) ; RFC2812
              (2 :RPL_YOURHOST) ; RFC2812
              (3 :RPL_CREATED) ; RFC2812
              (4 :RPL_MYINFO) ; RFC2812
              (4 :RPL_MYINFO) ; KineIRCd 
              (5 :RPL_BOUNCE) ; RFC2812
              (5 :RPL_ISUPPORT ) ; 
              (6 :RPL_MAP) ; Unreal
              (7 :RPL_MAPEND) ; Unreal
              (8 :RPL_SNOMASK) ; ircu
              (9 :RPL_STATMEMTOT) ; ircu
              (10 :RPL_BOUNCE) ; 
              (10 :RPL_STATMEM) ; ircu
              (14 :RPL_YOURCOOKIE) ; Hybrid?
              (15 :RPL_MAP) ; ircu
              (16 :RPL_MAPMORE) ; ircu
              (17 :RPL_MAPEND) ; ircu
              (42 :RPL_YOURID) ; IRCnet
              (43 :RPL_SAVENICK) ; IRCnet
              (50 :RPL_ATTEMPTINGJUNC) ; aircd
              (51 :RPL_ATTEMPTINGREROUTE) ; aircd
              (200 :RPL_TRACELINK) ; RFC1459
              (201 :RPL_TRACECONNECTING) ; RFC1459
              (202 :RPL_TRACEHANDSHAKE) ; RFC1459
              (203 :RPL_TRACEUNKNOWN) ; RFC1459
              (204 :RPL_TRACEOPERATOR) ; RFC1459
              (205 :RPL_TRACEUSER) ; RFC1459
              (206 :RPL_TRACESERVER) ; RFC1459
              (207 :RPL_TRACESERVICE) ; RFC2812
              (208 :RPL_TRACENEWTYPE) ; RFC1459
              (209 :RPL_TRACECLASS) ; RFC2812
              (210 :RPL_TRACERECONNECT) ; RFC2812
              (210 :RPL_STATS) ; aircd
              (211 :RPL_STATSLINKINFO) ; RFC1459
              (212 :RPL_STATSCOMMANDS) ; RFC1459
              (213 :RPL_STATSCLINE) ; RFC1459
              (214 :RPL_STATSNLINE) ; RFC1459
              (215 :RPL_STATSILINE) ; RFC1459
              (216 :RPL_STATSKLINE) ; RFC1459
              (217 :RPL_STATSQLINE) ; RFC1459
              (217 :RPL_STATSPLINE) ; ircu
              (218 :RPL_STATSYLINE) ; RFC1459
              (219 :RPL_ENDOFSTATS) ; RFC1459
              (220 :RPL_STATSPLINE) ; Hybrid
              (220 :RPL_STATSBLINE) ; Bahamut, Unreal
              (221 :RPL_UMODEIS) ; RFC1459
              (222 :RPL_MODLIST) ; 
              (222 :RPL_SQLINE_NICK) ; Unreal
              (222 :RPL_STATSBLINE) ; Bahamut
              (223 :RPL_STATSELINE) ; Bahamut
              (223 :RPL_STATSGLINE) ; Unreal
              (224 :RPL_STATSFLINE) ; Hybrid, Bahamut
              (224 :RPL_STATSTLINE) ; Unreal
              (225 :RPL_STATSDLINE) ; Hybrid
              (225 :RPL_STATSZLINE) ; Bahamut
              (225 :RPL_STATSELINE) ; Unreal
              (226 :RPL_STATSCOUNT) ; Bahamut
              (226 :RPL_STATSNLINE) ; Unreal
              (227 :RPL_STATSGLINE) ; Bahamut
              (227 :RPL_STATSVLINE) ; Unreal
              (228 :RPL_STATSQLINE) ; ircu
              (231 :RPL_SERVICEINFO) ; RFC1459
              (232 :RPL_ENDOFSERVICES) ; RFC1459
              (232 :RPL_RULES) ; Unreal
              (233 :RPL_SERVICE) ; RFC1459
              (234 :RPL_SERVLIST) ; RFC2812
              (235 :RPL_SERVLISTEND) ; RFC2812
              (236 :RPL_STATSVERBOSE) ; ircu
              (237 :RPL_STATSENGINE) ; ircu
              (238 :RPL_STATSFLINE) ; ircu
              (239 :RPL_STATSIAUTH) ; IRCnet
              (240 :RPL_STATSVLINE) ; RFC2812
              (240 :RPL_STATSXLINE) ; AustHex 
              (241 :RPL_STATSLLINE) ; RFC1459
              (242 :RPL_STATSUPTIME) ; RFC1459
              (243 :RPL_STATSOLINE) ; RFC1459
              (244 :RPL_STATSHLINE) ; RFC1459
              (245 :RPL_STATSSLINE) ; Bahamut, IRCnet, Hybrid
              (246 :RPL_STATSPING) ; RFC2812
              (246 :RPL_STATSTLINE) ; ircu
              (246 :RPL_STATSULINE) ; Hybrid
              (247 :RPL_STATSBLINE) ; RFC2812
              (247 :RPL_STATSXLINE) ; Hybrid, PTlink, Unreal
              (247 :RPL_STATSGLINE) ; ircu
              (248 :RPL_STATSULINE) ; ircu
              (248 :RPL_STATSDEFINE) ; IRCnet
              (249 :RPL_STATSULINE) ; 
              (249 :RPL_STATSDEBUG) ; Hybrid
              (250 :RPL_STATSDLINE) ; RFC2812
              (250 :RPL_STATSCONN) ; ircu, Unreal
              (251 :RPL_LUSERCLIENT) ; RFC1459
              (252 :RPL_LUSEROP) ; RFC1459
              (253 :RPL_LUSERUNKNOWN) ; RFC1459
              (254 :RPL_LUSERCHANNELS) ; RFC1459
              (255 :RPL_LUSERME) ; RFC1459
              (256 :RPL_ADMINME) ; RFC1459
              (257 :RPL_ADMINLOC1) ; RFC1459
              (258 :RPL_ADMINLOC2) ; RFC1459
              (259 :RPL_ADMINEMAIL) ; RFC1459
              (261 :RPL_TRACELOG) ; RFC1459
              (262 :RPL_TRACEPING) ; 
              (262 :RPL_TRACEEND) ; RFC2812
              (263 :RPL_TRYAGAIN) ; RFC2812
              (265 :RPL_LOCALUSERS) ; aircd, Hybrid, Hybrid, Bahamut
              (266 :RPL_GLOBALUSERS) ; aircd, Hybrid, Hybrid, Bahamut
              (267 :RPL_START_NETSTAT) ; aircd
              (268 :RPL_NETSTAT) ; aircd
              (269 :RPL_END_NETSTAT) ; aircd
              (270 :RPL_PRIVS) ; ircu
              (271 :RPL_SILELIST) ; ircu
              (272 :RPL_ENDOFSILELIST) ; ircu
              (273 :RPL_NOTIFY) ; aircd
              (274 :RPL_ENDNOTIFY) ; aircd
              (274 :RPL_STATSDELTA) ; IRCnet
              (275 :RPL_STATSDLINE) ; ircu, Ultimate
              (276 :RPL_VCHANEXIST) ; 
              (277 :RPL_VCHANLIST) ; 
              (278 :RPL_VCHANHELP) ; 
              (280 :RPL_GLIST) ; ircu
              (281 :RPL_ENDOFGLIST) ; ircu
              (281 :RPL_ACCEPTLIST) ; 
              (282 :RPL_ENDOFACCEPT) ; 
              (282 :RPL_JUPELIST) ; ircu
              (283 :RPL_ALIST) ; 
              (283 :RPL_ENDOFJUPELIST) ; ircu
              (284 :RPL_ENDOFALIST) ; 
              (284 :RPL_FEATURE) ; ircu
              (285 :RPL_GLIST_HASH) ; 
              (285 :RPL_CHANINFO_HANDLE) ; aircd
              (285 :RPL_NEWHOSTIS) ; QuakeNet
              (286 :RPL_CHANINFO_USERS) ; aircd
              (286 :RPL_CHKHEAD) ; QuakeNet
              (287 :RPL_CHANINFO_CHOPS) ; aircd
              (287 :RPL_CHANUSER) ; QuakeNet
              (288 :RPL_CHANINFO_VOICES) ; aircd
              (288 :RPL_PATCHHEAD) ; QuakeNet
              (289 :RPL_CHANINFO_AWAY) ; aircd
              (289 :RPL_PATCHCON) ; QuakeNet
              (290 :RPL_CHANINFO_OPERS) ; aircd
              (290 :RPL_HELPHDR) ; Unreal
              (290 :RPL_DATASTR) ; QuakeNet
              (291 :RPL_CHANINFO_BANNED) ; aircd
              (291 :RPL_HELPOP) ; Unreal
              (291 :RPL_ENDOFCHECK) ; QuakeNet
              (292 :RPL_CHANINFO_BANS) ; aircd
              (292 :RPL_HELPTLR) ; Unreal
              (293 :RPL_CHANINFO_INVITE) ; aircd
              (293 :RPL_HELPHLP) ; Unreal
              (294 :RPL_CHANINFO_INVITES) ; aircd
              (294 :RPL_HELPFWD) ; Unreal
              (295 :RPL_CHANINFO_KICK) ; aircd
              (295 :RPL_HELPIGN) ; Unreal
              (296 :RPL_CHANINFO_KICKS) ; aircd
              (299 :RPL_END_CHANINFO) ; aircd
              (300 :RPL_NONE) ; RFC1459
              (301 :RPL_AWAY) ; RFC1459
              (301 :RPL_AWAY) ; KineIRCd
              (302 :RPL_USERHOST) ; RFC1459
              (303 :RPL_ISON) ; RFC1459
              (304 :RPL_TEXT) ; 
              (305 :RPL_UNAWAY) ; RFC1459
              (306 :RPL_NOWAWAY) ; RFC1459
              (307 :RPL_USERIP) ; 
              (307 :RPL_WHOISREGNICK) ; Bahamut, Unreal
              (307 :RPL_SUSERHOST) ; AustHex 
              (308 :RPL_NOTIFYACTION) ; aircd
              (308 :RPL_WHOISADMIN) ; Bahamut
              (308 :RPL_RULESSTART) ; Unreal
              (309 :RPL_NICKTRACE) ; aircd
              (309 :RPL_WHOISSADMIN) ; Bahamut
              (309 :RPL_ENDOFRULES) ; Unreal
              (309 :RPL_WHOISHELPER) ; AustHex 
              (310 :RPL_WHOISSVCMSG) ; Bahamut
              (310 :RPL_WHOISHELPOP) ; Unreal
              (310 :RPL_WHOISSERVICE) ; AustHex 
              (311 :RPL_WHOISUSER) ; RFC1459
              (312 :RPL_WHOISSERVER) ; RFC1459
              (313 :RPL_WHOISOPERATOR) ; RFC1459
              (314 :RPL_WHOWASUSER) ; RFC1459
              (315 :RPL_ENDOFWHO) ; RFC1459
              (316 :RPL_WHOISCHANOP) ; RFC1459
              (317 :RPL_WHOISIDLE) ; RFC1459
              (318 :RPL_ENDOFWHOIS) ; RFC1459
              (319 :RPL_WHOISCHANNELS) ; RFC1459
              (320 :RPL_WHOISVIRT) ; AustHex 
              (320 :RPL_WHOIS_HIDDEN) ; Anothernet
              (320 :RPL_WHOISSPECIAL) ; Unreal
              (321 :RPL_LISTSTART) ; RFC1459
              (322 :RPL_LIST) ; RFC1459
              (323 :RPL_LISTEND) ; RFC1459
              (324 :RPL_CHANNELMODEIS) ; RFC1459
              (325 :RPL_UNIQOPIS) ; RFC2812
              (325 :RPL_CHANNELPASSIS) ; 
              (326 :RPL_NOCHANPASS) ; 
              (327 :RPL_CHPASSUNKNOWN) ; 
              (328 :RPL_CHANNEL_URL) ; Bahamut, AustHex
              (329 :RPL_CREATIONTIME) ; Bahamut
              (330 :RPL_WHOWAS_TIME) ; 
              (330 :RPL_WHOISACCOUNT) ; ircu
              (331 :RPL_NOTOPIC) ; RFC1459
              (332 :RPL_TOPIC) ; RFC1459
              (333 :RPL_TOPICWHOTIME) ; ircu
              (334 :RPL_LISTUSAGE) ; ircu
              (334 :RPL_COMMANDSYNTAX) ; Bahamut
              (334 :RPL_LISTSYNTAX) ; Unreal
              (335 :RPL_WHOISBOT) ; Unreal
              (338 :RPL_CHANPASSOK) ; 
              (338 :RPL_WHOISACTUALLY) ; ircu, Bahamut
              (339 :RPL_BADCHANPASS) ; 
              (340 :RPL_USERIP) ; ircu
              (341 :RPL_INVITING) ; RFC1459
              (342 :RPL_SUMMONING) ; RFC1459
              (345 :RPL_INVITED) ; GameSurge 
              (346 :RPL_INVITELIST) ; RFC2812
              (347 :RPL_ENDOFINVITELIST) ; RFC2812
              (348 :RPL_EXCEPTLIST) ; RFC2812
              (349 :RPL_ENDOFEXCEPTLIST) ; RFC2812
              (351 :RPL_VERSION) ; RFC1459
              (352 :RPL_WHOREPLY) ; RFC1459
              (353 :RPL_NAMREPLY) ; RFC1459
              (354 :RPL_WHOSPCRPL) ; ircu
              (355 :RPL_NAMREPLY_) ; QuakeNet
              (357 :RPL_MAP) ; AustHex
              (358 :RPL_MAPMORE) ; AustHex
              (359 :RPL_MAPEND) ; AustHex
              (361 :RPL_KILLDONE) ; RFC1459
              (362 :RPL_CLOSING) ; RFC1459
              (363 :RPL_CLOSEEND) ; RFC1459
              (364 :RPL_LINKS) ; RFC1459
              (365 :RPL_ENDOFLINKS) ; RFC1459
              (366 :RPL_ENDOFNAMES) ; RFC1459
              (367 :RPL_BANLIST) ; RFC1459
              (368 :RPL_ENDOFBANLIST) ; RFC1459
              (369 :RPL_ENDOFWHOWAS) ; RFC1459
              (371 :RPL_INFO) ; RFC1459
              (372 :RPL_MOTD) ; RFC1459
              (373 :RPL_INFOSTART) ; RFC1459
              (374 :RPL_ENDOFINFO) ; RFC1459
              (375 :RPL_MOTDSTART) ; RFC1459
              (376 :RPL_ENDOFMOTD) ; RFC1459
              (377 :RPL_KICKEXPIRED) ; aircd
              (377 :RPL_SPAM) ; AustHex 
              (378 :RPL_BANEXPIRED) ; aircd
              (378 :RPL_WHOISHOST) ; Unreal
              (378 :RPL_MOTD) ; AustHex
              (379 :RPL_KICKLINKED) ; aircd
              (379 :RPL_WHOISMODES) ; Unreal
              (380 :RPL_BANLINKED) ; aircd
              (380 :RPL_YOURHELPER) ; AustHex
              (381 :RPL_YOUREOPER) ; RFC1459
              (382 :RPL_REHASHING) ; RFC1459
              (383 :RPL_YOURESERVICE) ; RFC2812
              (384 :RPL_MYPORTIS) ; RFC1459
              (385 :RPL_NOTOPERANYMORE) ; AustHex, Hybrid, Unreal
              (386 :RPL_QLIST) ; Unreal
              (386 :RPL_IRCOPS) ; Ultimate
              (387 :RPL_ENDOFQLIST) ; Unreal
              (387 :RPL_ENDOFIRCOPS) ; Ultimate
              (388 :RPL_ALIST) ; Unreal
              (389 :RPL_ENDOFALIST) ; Unreal
              (391 :RPL_TIME) ; RFC1459
              (391 :RPL_TIME) ; ircu
              (391 :RPL_TIME) ; bdq-ircd
              (391 :RPL_TIME) ; 
              (392 :RPL_USERSSTART) ; RFC1459
              (393 :RPL_USERS) ; RFC1459
              (394 :RPL_ENDOFUSERS) ; RFC1459
              (395 :RPL_NOUSERS) ; RFC1459
              (396 :RPL_HOSTHIDDEN) ; Undernet
              (400 :ERR_UNKNOWNERROR) ; 
              (401 :ERR_NOSUCHNICK) ; RFC1459
              (402 :ERR_NOSUCHSERVER) ; RFC1459
              (403 :ERR_NOSUCHCHANNEL) ; RFC1459
              (404 :ERR_CANNOTSENDTOCHAN) ; RFC1459
              (405 :ERR_TOOMANYCHANNELS) ; RFC1459
              (406 :ERR_WASNOSUCHNICK) ; RFC1459
              (407 :ERR_TOOMANYTARGETS) ; RFC1459
              (408 :ERR_NOSUCHSERVICE) ; RFC2812
              (408 :ERR_NOCOLORSONCHAN) ; Bahamut
              (409 :ERR_NOORIGIN) ; RFC1459
              (411 :ERR_NORECIPIENT) ; RFC1459
              (412 :ERR_NOTEXTTOSEND) ; RFC1459
              (413 :ERR_NOTOPLEVEL) ; RFC1459
              (414 :ERR_WILDTOPLEVEL) ; RFC1459
              (415 :ERR_BADMASK) ; RFC2812
              (416 :ERR_TOOMANYMATCHES) ; IRCnet
              (416 :ERR_QUERYTOOLONG) ; ircu
              (419 :ERR_LENGTHTRUNCATED) ; aircd
              (421 :ERR_UNKNOWNCOMMAND) ; RFC1459
              (422 :ERR_NOMOTD) ; RFC1459
              (423 :ERR_NOADMININFO) ; RFC1459
              (424 :ERR_FILEERROR) ; RFC1459
              (425 :ERR_NOOPERMOTD) ; Unreal
              (429 :ERR_TOOMANYAWAY) ; Bahamut
              (430 :ERR_EVENTNICKCHANGE) ; AustHex 
              (431 :ERR_NONICKNAMEGIVEN) ; RFC1459
              (432 :ERR_ERRONEUSNICKNAME) ; RFC1459
              (433 :ERR_NICKNAMEINUSE) ; RFC1459
              (434 :ERR_SERVICENAMEINUSE) ; AustHex?
              (434 :ERR_NORULES) ; Unreal, Ultimate
              (435 :ERR_SERVICECONFUSED) ; Unreal
              (435 :ERR_BANONCHAN) ; Bahamut
              (436 :ERR_NICKCOLLISION) ; RFC1459
              (437 :ERR_UNAVAILRESOURCE) ; RFC2812
              (437 :ERR_BANNICKCHANGE) ; ircu
              (438 :ERR_NICKTOOFAST) ; ircu
              (438 :ERR_DEAD) ; IRCnet
              (439 :ERR_TARGETTOOFAST) ; ircu
              (440 :ERR_SERVICESDOWN) ; Bahamut, Unreal
              (441 :ERR_USERNOTINCHANNEL) ; RFC1459
              (442 :ERR_NOTONCHANNEL) ; RFC1459
              (443 :ERR_USERONCHANNEL) ; RFC1459
              (444 :ERR_NOLOGIN) ; RFC1459
              (445 :ERR_SUMMONDISABLED) ; RFC1459
              (446 :ERR_USERSDISABLED) ; RFC1459
              (447 :ERR_NONICKCHANGE) ; Unreal
              (449 :ERR_NOTIMPLEMENTED) ; Undernet
              (451 :ERR_NOTREGISTERED) ; RFC1459
              (452 :ERR_IDCOLLISION) ; 
              (453 :ERR_NICKLOST) ; 
              (455 :ERR_HOSTILENAME) ; Unreal
              (456 :ERR_ACCEPTFULL) ; 
              (457 :ERR_ACCEPTEXIST) ; 
              (458 :ERR_ACCEPTNOT) ; 
              (459 :ERR_NOHIDING) ; Unreal
              (460 :ERR_NOTFORHALFOPS) ; Unreal
              (461 :ERR_NEEDMOREPARAMS) ; RFC1459
              (462 :ERR_ALREADYREGISTERED) ; RFC1459
              (463 :ERR_NOPERMFORHOST) ; RFC1459
              (464 :ERR_PASSWDMISMATCH) ; RFC1459
              (465 :ERR_YOUREBANNEDCREEP) ; RFC1459
              (466 :ERR_YOUWILLBEBANNED) ; RFC1459
              (467 :ERR_KEYSET) ; RFC1459
              (468 :ERR_INVALIDUSERNAME) ; ircu
              (468 :ERR_ONLYSERVERSCANCHANGE) ; Bahamut, Unreal
              (469 :ERR_LINKSET) ; Unreal
              (470 :ERR_LINKCHANNEL) ; Unreal
              (470 :ERR_KICKEDFROMCHAN) ; aircd
              (471 :ERR_CHANNELISFULL) ; RFC1459
              (472 :ERR_UNKNOWNMODE) ; RFC1459
              (473 :ERR_INVITEONLYCHAN) ; RFC1459
              (474 :ERR_BANNEDFROMCHAN) ; RFC1459
              (475 :ERR_BADCHANNELKEY) ; RFC1459
              (476 :ERR_BADCHANMASK) ; RFC2812
              (477 :ERR_NOCHANMODES) ; RFC2812
              (477 :ERR_NEEDREGGEDNICK) ; Bahamut, ircu, Unreal
              (478 :ERR_BANLISTFULL) ; RFC2812
              (479 :ERR_BADCHANNAME) ; Hybrid
              (479 :ERR_LINKFAIL) ; Unreal
              (480 :ERR_NOULINE) ; AustHex 
              (480 :ERR_CANNOTKNOCK) ; Unreal
              (481 :ERR_NOPRIVILEGES) ; RFC1459
              (482 :ERR_CHANOPRIVSNEEDED) ; RFC1459
              (483 :ERR_CANTKILLSERVER) ; RFC1459
              (484 :ERR_RESTRICTED) ; RFC2812
              (484 :ERR_ISCHANSERVICE) ; Undernet
              (484 :ERR_DESYNC) ; Bahamut, Hybrid, PTlink
              (484 :ERR_ATTACKDENY) ; Unreal
              (485 :ERR_UNIQOPRIVSNEEDED) ; RFC2812
              (485 :ERR_KILLDENY) ; Unreal
              (485 :ERR_CANTKICKADMIN) ; PTlink
              (485 :ERR_ISREALSERVICE) ; QuakeNet
              (486 :ERR_NONONREG) ; 
              (486 :ERR_HTMDISABLED) ; Unreal
              (486 :ERR_ACCOUNTONLY) ; QuakeNet
              (487 :ERR_CHANTOORECENT) ; IRCnet
              (487 :ERR_MSGSERVICES) ; Bahamut
              (488 :ERR_TSLESSCHAN) ; IRCnet
              (489 :ERR_VOICENEEDED) ; Undernet
              (489 :ERR_SECUREONLYCHAN) ; Unreal
              (491 :ERR_NOOPERHOST) ; RFC1459
              (492 :ERR_NOSERVICEHOST) ; RFC1459
              (493 :ERR_NOFEATURE) ; ircu
              (494 :ERR_BADFEATURE) ; ircu
              (495 :ERR_BADLOGTYPE) ; ircu
              (496 :ERR_BADLOGSYS) ; ircu
              (497 :ERR_BADLOGVALUE) ; ircu
              (498 :ERR_ISOPERLCHAN) ; ircu
              (499 :ERR_CHANOWNPRIVNEEDED) ; Unreal
              (501 :ERR_UMODEUNKNOWNFLAG) ; RFC1459
              (502 :ERR_USERSDONTMATCH) ; RFC1459
              (503 :ERR_GHOSTEDCLIENT) ; Hybrid
              (503 :ERR_VWORLDWARN) ; AustHex 
              (504 :ERR_USERNOTONSERV) ; 
              (511 :ERR_SILELISTFULL) ; ircu
              (512 :ERR_TOOMANYWATCH) ; Bahamut
              (513 :ERR_BADPING) ; ircu
              (514 :ERR_INVALID_ERROR) ; ircu
              (514 :ERR_TOOMANYDCC) ; Bahamut
              (515 :ERR_BADEXPIRE) ; ircu
              (516 :ERR_DONTCHEAT) ; ircu
              (517 :ERR_DISABLED) ; ircu
              (518 :ERR_NOINVITE) ; Unreal
              (518 :ERR_LONGMASK) ; ircu
              (519 :ERR_ADMONLY) ; Unreal
              (519 :ERR_TOOMANYUSERS) ; ircu
              (520 :ERR_OPERONLY) ; Unreal
              (520 :ERR_MASKTOOWIDE) ; ircu
              (520 :ERR_WHOTRUNC) ; AustHex 
              (521 :ERR_LISTSYNTAX) ; Bahamut
              (522 :ERR_WHOSYNTAX) ; Bahamut
              (523 :ERR_WHOLIMEXCEED) ; Bahamut
              (524 :ERR_QUARANTINED) ; ircu
              (524 :ERR_OPERSPVERIFY) ; Unreal
              (525 :ERR_REMOTEPFX ) ; CAPAB USERCMDPFX 
              (526 :ERR_PFXUNROUTABLE ) ; CAPAB USERCMDPFX 
              (550 :ERR_BADHOSTMASK) ; QuakeNet
              (551 :ERR_HOSTUNAVAIL) ; QuakeNet
              (552 :ERR_USINGSLINE) ; QuakeNet
              (553 :ERR_STATSSLINE) ; QuakeNet
              (600 :RPL_LOGON) ; Bahamut, Unreal
              (601 :RPL_LOGOFF) ; Bahamut, Unreal
              (602 :RPL_WATCHOFF) ; Bahamut, Unreal
              (603 :RPL_WATCHSTAT) ; Bahamut, Unreal
              (604 :RPL_NOWON) ; Bahamut, Unreal
              (605 :RPL_NOWOFF) ; Bahamut, Unreal
              (606 :RPL_WATCHLIST) ; Bahamut, Unreal
              (607 :RPL_ENDOFWATCHLIST) ; Bahamut, Unreal
              (608 :RPL_WATCHCLEAR) ; Ultimate
              (610 :RPL_MAPMORE) ; Unreal
              (610 :RPL_ISOPER) ; Ultimate
              (611 :RPL_ISLOCOP) ; Ultimate
              (612 :RPL_ISNOTOPER) ; Ultimate
              (613 :RPL_ENDOFISOPER) ; Ultimate
              (615 :RPL_MAPMORE) ; PTlink
              (615 :RPL_WHOISMODES) ; Ultimate
              (616 :RPL_WHOISHOST) ; Ultimate
              (617 :RPL_DCCSTATUS) ; Bahamut
              (617 :RPL_WHOISBOT) ; Ultimate
              (618 :RPL_DCCLIST) ; Bahamut
              (619 :RPL_ENDOFDCCLIST) ; Bahamut
              (619 :RPL_WHOWASHOST) ; Ultimate
              (620 :RPL_DCCINFO) ; Bahamut
              (620 :RPL_RULESSTART) ; Ultimate
              (621 :RPL_RULES) ; Ultimate
              (622 :RPL_ENDOFRULES) ; Ultimate
              (623 :RPL_MAPMORE) ; Ultimate
              (624 :RPL_OMOTDSTART) ; Ultimate
              (625 :RPL_OMOTD) ; Ultimate
              (626 :RPL_ENDOFO) ; Ultimate 
              (630 :RPL_SETTINGS) ; Ultimate
              (631 :RPL_ENDOFSETTINGS) ; Ultimate
              (640 :RPL_DUMPING) ; Unreal
              (641 :RPL_DUMPRPL) ; Unreal
              (642 :RPL_EODUMP) ; Unreal
              (660 :RPL_TRACEROUTE_HOP) ; KineIRCd 
              (661 :RPL_TRACEROUTE_START) ; KineIRCd 
              (662 :RPL_MODECHANGEWARN) ; KineIRCd 
              (663 :RPL_CHANREDIR) ; KineIRCd 
              (664 :RPL_SERVMODEIS) ; KineIRCd 
              (665 :RPL_OTHERUMODEIS) ; KineIRCd 
              (666 :RPL_ENDOF_GENERIC) ; KineIRCd 
              (670 :RPL_WHOWASDETAILS) ; KineIRCd 
              (671 :RPL_WHOISSECURE) ; KineIRCd 
              (672 :RPL_UNKNOWNMODES) ; Ithildin 
              (673 :RPL_CANNOTSETMODES) ; Ithildin 
              (678 :RPL_LUSERSTAFF) ; KineIRCd 
              (679 :RPL_TIMEONSERVERIS) ; KineIRCd 
              (682 :RPL_NETWORKS ) ; KineIRCd 
              (687 :RPL_YOURLANGUAGEIS ) ; KineIRCd 
              (688 :RPL_LANGUAGE ) ; KineIRCd 
              (689 :RPL_WHOISSTAFF) ; KineIRCd 
              (690 :RPL_WHOISLANGUAGE ) ; KineIRCd 
              (702 :RPL_MODLIST) ; RatBox
              (703 :RPL_ENDOFMODLIST) ; RatBox
              (704 :RPL_HELPSTART) ; RatBox
              (705 :RPL_HELPTXT) ; RatBox
              (706 :RPL_ENDOFHELP) ; RatBox
              (708 :RPL_ETRACEFULL) ; RatBox
              (709 :RPL_ETRACE) ; RatBox
              (710 :RPL_KNOCK) ; RatBox
              (711 :RPL_KNOCKDLVR) ; RatBox
              (712 :ERR_TOOMANYKNOCK) ; RatBox
              (713 :ERR_CHANOPEN) ; RatBox
              (714 :ERR_KNOCKONCHAN) ; RatBox
              (715 :ERR_KNOCKDISABLED) ; RatBox
              (716 :RPL_TARGUMODEG) ; RatBox
              (717 :RPL_TARGNOTIFY) ; RatBox
              (718 :RPL_UMODEGMSG) ; RatBox
              (720 :RPL_OMOTDSTART) ; RatBox
              (721 :RPL_OMOTD) ; RatBox
              (722 :RPL_ENDOFOMOTD) ; RatBox
              (723 :ERR_NOPRIVS) ; RatBox
              (724 :RPL_TESTMARK) ; RatBox
              (725 :RPL_TESTLINE) ; RatBox
              (726 :RPL_NOTESTLINE) ; RatBox
              (771 :RPL_XINFO) ; Ithildin 
              (773 :RPL_XINFOSTART) ; Ithildin 
              (774 :RPL_XINFOEND) ; Ithildin 
              (972 :ERR_CANNOTDOCOMMAND) ; Unreal
              (973 :ERR_CANNOTCHANGEUMODE) ; KineIRCd 
              (974 :ERR_CANNOTCHANGECHANMODE) ; KineIRCd 
              (975 :ERR_CANNOTCHANGESERVERMODE) ; KineIRCd 
              (976 :ERR_CANNOTSENDTONICK) ; KineIRCd 
              (977 :ERR_UNKNOWNSERVERMODE) ; KineIRCd 
              (979 :ERR_SERVERMODELOCK) ; KineIRCd 
              (980 :ERR_BADCHARENCODING) ; KineIRCd 
              (981 :ERR_TOOMANYLANGUAGES ) ; KineIRCd 
              (982 :ERR_NOLANGUAGE ) ; KineIRCd 
              (983 :ERR_TEXTTOOSHORT) ; KineIRCd 
              (999 :ERR_NUMERIC_ERR) ; Bahamut
              )))))
