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
  :version "0.9.3"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Irc bot with various functions."
  :serial T
  :components ((:file "package")
               (:file "globals")
               (:file "conditions")
               (:file "reply-codes")
               (:file "config")
               (:file "events")
               (:file "module")
               (:file "server")
               (:file "commands")
               (:file "colleen")
               (:file "launcher")
               (:module "modules"
                        :components ((:file "authenticate")
                                     (:file "blants")
                                     (:file "chatlog")
                                     (:file "dramatica")
                                     (:file "essentials")
                                     (:file "markov")
                                     (:file "medals")
                                     (:file "notify")
                                     (:file "rss")
                                     (:file "search")
                                     (:file "silly")
                                     (:file "stevenchan")
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
               :uuid))

(defsystem colleen-doc
  :name "Colleen Doc"
  :components ((:file "documentation"))
  :depends-on (:colleen :lquery-doc))
