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
  :version "0.0.1"
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
                        :components ((:file "authenticate"))))
  :depends-on (:bordeaux-threads
               :drakma
               :cl-json
               :verbose
               :split-sequence
               :local-time
               :lquery
               :alexandria
               :usocket))
