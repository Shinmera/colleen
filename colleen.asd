#|
  This file is a part of Colleen
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.colleen.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.colleen.asdf)

(defsystem colleen
  :name "Colleen IRC Bot"
  :version "2.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :homepage "http://github.com/Shinmera/colleen"
  :description "IRC bot with a modular framework."
  :serial T
  :components ((:file "package")
               (:file "globals")
               (:file "conditions")
               (:file "irc-codes")
               (:file "config")
               (:file "toolkit")
               (:file "module")
               (:file "module-storage")
               (:file "event")
               (:file "event-priority")
               (:file "event-handler")
               (:file "client")
               (:file "command-handler")
               (:file "time-handler")
               (:file "irc-commands")
               (:file "irc-events")
               (:file "asdf-extra")
               (:file "launcher")
               (:module "modules"
                :components ((:file "system-definitions")
                             (:file "core"))))
  :depends-on (:bordeaux-threads 
               :universal-config
               :verbose
               :split-sequence
               :alexandria
               :usocket
               :flexi-streams
               :uuid
               :cl-ppcre
               :trivial-arguments))
