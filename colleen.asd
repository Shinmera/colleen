#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem colleen
  :version "2.2.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :homepage "https://Shinmera.github.io/colleen/"
  :bug-tracker "https://github.com/Shinmera/colleen/issues"
  :source-control (:git "https://github.com/Shinmera/colleen.git")
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
               :usocket
               :flexi-streams
               :uuid
               :cl-ppcre
               :trivial-arguments))
