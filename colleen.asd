#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.colleen.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.colleen.asdf)

(defsystem colleen
  :name "Colleen IRC Bot"
  :version "0.9.6"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "IRC bot with a modular framework."
  :serial T
  :components ((:file "package")
               (:file "globals")
               (:file "conditions")
               (:file "reply-codes")
               (:file "config")
               (:file "event")
               (:file "module")
               (:file "server")
               (:file "commands")
               (:file "events")
               (:file "launcher")
               (:module "modules"
                :components ((:file "authenticate")
                             (:file "backup")
                             (:file "blants")
                             (:file "chatlog")
                             (:file "convert")
                             (:file "core")
                             (:file "counter")
                             ;(:file "dramatica")
                             (:file "dictionary")
                             (:file "emoticon")
                             (:file "essentials")
                             (:file "eval")
                             (:file "google")
                             (:file "google-lng")
                             (:file "markov")
                             (:file "medals")
                             (:file "notify")
                             (:file "rss")
                             (:file "rules")
                             (:file "search")
                             (:file "shiritori")
                             (:file "silly")
                             (:file "stevenchan")
                             (:file "syslog")
                             (:file "weather"))))
  :depends-on (:bordeaux-threads
               :drakma
               :cl-json :yason
               :verbose
               :split-sequence
               :local-time
               :lquery
               :alexandria
               :usocket
               :uuid
               ;; module deps
               :ironclad
               :ieee-floats
               :parse-number
               :clsql
               :clsql-mysql
               :cl-wiki
               :cl-fad
               ;:xencl
               :trivial-timers))

(defsystem colleen-doc
  :name "Colleen Doc"
  :components ((:file "documentation"))
  :depends-on (:colleen :lquery-doc))
