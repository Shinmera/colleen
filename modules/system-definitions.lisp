#|
 This file is a part of Colleen
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(define-module-system auth ("authenticate"))
(define-module-system backup ("backup") (:trivial-timers :cl-fad))
(define-module-system chatlog ("chatlog") (:clsql :clsql-mysql))
(define-module-system convert ("convert") (:drakma :ieee-floats :ironclad :parse-number :cl-json))
(define-module-system core ("core"))
(define-module-system counter ("counter") (:yason))
(define-module-system dictionary ("dictionary") (:yason :alexandria))
(define-module-system dramatica ("dramatica") (:xencl :cl-wiki))
(define-module-system emoticon ("emoticon") (:yason))
(define-module-system essentials ("essentials") (:uiop :local-time :alexandria))
(define-module-system eval ("eval"))
(define-module-system google ("google" "google-lng") (:split-sequence :drakma :cl-json))
(define-module-system markov ("markov") (:cl-json))
(define-module-system medals ("medals") (:yason))
(define-module-system notify ("notify") (:yason))
(define-module-system rss ("rss") (:lquery :drakma :cl-json))
(define-module-system rules ("rules") (:yason :alexandria))
(define-module-system search ("search") (:lquery :split-sequence :drakma :cl-wiki))
(define-module-system shiritori ("shiritori"))
(define-module-system silly ("silly") (:cl-ppcre))
(define-module-system stevenchan ("stevenchan") (:lquery :drakma :alexandria))
(define-module-system syslog ("syslog"))
(define-module-system twitter ("twitter") (:chirp :yason))
(define-module-system weather ("weather") (:drakma :cl-json))
