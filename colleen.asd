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
  :version "2.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "IRC bot with a modular framework."
  :serial T
  :components ((:file "package")
               (:file "globals")
               (:file "conditions")
               (:file "irc-codes")
               (:file "config")
               (:file "toolkit")
               (:file "module")
               (:file "event")
               (:file "event-priority")
               (:file "event-handler")
               (:file "command-handler")
               (:file "client")
               (:file "irc-commands")
               (:file "irc-events")
               (:file "asdf-extra")
               (:file "launcher")
               (:module "modules"
                :components ((:file "system-definitions"))))
  :depends-on (:bordeaux-threads 
               :yason
               :verbose
               :split-sequence
               :alexandria
               :usocket
               :flexi-streams
               :uuid
               :cl-ppcre))

;; (defsystem colleen-doc
;;   :name "Colleen Doc"
;;   :components ((:file "documentation"))
;;   :depends-on (:colleen :lquery-doc))
