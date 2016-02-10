#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(define-module-system auth ("authenticate"))
(define-module-system backup ("backup") (:cl-fad))
(define-module-system chanblock ("chanblock"))
(define-module-system chatlog-pg ("chatlog-pg") (:postmodern :local-time))
(define-module-system chatlog ("chatlog") (:clsql :clsql-mysql :local-time))
(define-module-system convert ("convert") (:drakma :ieee-floats :ironclad :parse-number :cl-json))
(define-module-system core ("core"))
(define-module-system counter ("counter"))
(define-module-system dictionary ("dictionary") (:alexandria :lquery :cl-wiki))
(define-module-system dramatica ("dramatica") (:cl-wiki))
(define-module-system emoticon ("emoticon"))
(define-module-system essentials ("essentials") (:uiop :local-time :alexandria))
(define-module-system eval ("eval"))
(define-module-system google ("google" "google-lng") (:split-sequence :drakma :cl-json :plump))
(define-module-system markov-twitter-bridge ("markov-twitter-bridge") (:co-markov :chirp))
(define-module-system markov ("markov"))
(define-module-system medals ("medals"))
(define-module-system mentions ("mentions") (:cl-ppcre :local-time))
(define-module-system notify ("notify") (:local-time))
(define-module-system profile ("profile") (:local-time))
(define-module-system rss ("rss") (:lquery :drakma :cl-json))
(define-module-system rules ("rules") (:alexandria))
(define-module-system search ("search") (:lquery :split-sequence :drakma :cl-wiki))
(define-module-system shiritori ("shiritori"))
(define-module-system silly ("silly") (:cl-ppcre :drakma :lquery :cl-acronyms))
(define-module-system stands4 ("stands4") (:drakma :lquery))
(define-module-system stevenchan ("stevenchan") (:lquery :drakma :alexandria))
(define-module-system syslog ("syslog"))
(define-module-system throttle ("throttle"))
(define-module-system topic ("topic") (:cl-ppcre))
(define-module-system twitch ("twitch") (:drakma :jsown))
(define-module-system twitter ("twitter") (:chirp :cl-ppcre))
(define-module-system urlinfo ("urlinfo") (:drakma :cl-ppcre :lquery))
(define-module-system weather ("weather") (:drakma :cl-json))
(define-module-system welcome ("welcome") (:cl-ppcre))
