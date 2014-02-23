#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defvar *servers* (make-hash-table :synchronized T) "Table containing the IRC server instances.")
(defvar *bot-modules* (make-hash-table :synchronized T) "Global module table consisting of name->instance pairs.")
(defvar *event-map* (make-hash-table :synchronized T :test 'equal) "Global event map for event codes to event classes.")
(defvar *conf-file* NIL "Pathname pointing to the config file being used.")
(defvar *conf* NIL "ALIST structure of the global configuration.")
(defvar *debugger* NIL "Boolean indicating whether to invoke the debugger on an unhandled condition.")

(defvar *current-server* "Special variable containing the server in the current thread context.")

(defvar *user-regex* (cl-ppcre:create-scanner "(.+)!(.+)@(.+)"))
