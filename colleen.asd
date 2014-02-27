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
               :uuid
               :cl-ppcre))

(defsystem colleen-doc
  :name "Colleen Doc"
  :components ((:file "documentation"))
  :depends-on (:colleen :lquery-doc))
