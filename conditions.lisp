#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(define-condition module-error (error) ()
  (:documentation "Condition superclass for module related errors."))

(define-condition invalid-arguments (module-error)
  ((%command :initarg :command :reader command)
   (%argslist :initarg :argslist :reader argslist)
   (%expected :initarg :expected :reader expected))
  (:documentation "Condition raised when a command is called with an invalid arguments structure.")
  (:report (lambda (c s)
             (format s "Invalid arguments for command ~s: ~s Expected form: ~s" (command c) (argslist c) (expected c)))))

(define-condition not-authorized (module-error)
  ((%event :initarg :event :reader event))
  (:documentation "Condition raised when a user tried to execute a command he isn't authorized for.")
  (:report (lambda (c s)
             (format s "User is not authorized to execute command ~a" (event c)))))

(define-condition network-error (error) 
  ((%server :initarg :server :reader failed-server))
  (:documentation "Condition superclass for network releated errors.")
  (:report (lambda (c s)
             (format s "Network error occurred in server ~a" (failed-server c)))))

(define-condition disconnect (network-error) ()
  (:documentation "Condition to disconnect and leave the handler loops.")
  (:report (lambda (c s)
             (format s "Requested disconnect from server ~a" (failed-server c)))))

(define-condition connection-failed (network-error)
  ((%error :initarg :error :reader initial-error))
  (:documentation "Condition when the connection attempt failed for some reason. The original error is included.")
  (:report (lambda (c s)
             (format s "Connection to server ~a failed: ~a" (failed-server c) (initial-error c)))))

(define-condition ping-timeout (network-error) ()
  (:documentation "Condition signalled when a ping-timeout is noticed.")
  (:report (lambda (c s)
             (format s "Ping timeout signalled for server ~a" (failed-server c)))))

(define-condition module-stop (condition) ()
  (:documentation "Condition signalled when a thread is interrupted due to a module-stop.")
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Immediate module stop signalled."))))

(define-condition module-system-not-found (error)
  ((%name :initarg :name :reader name))
  (:documentation "Condition signalled when LOAD-MODULE is called on an unknown module system.")
  (:report (lambda (c s)
             (format s "Failed to find ASDF system for ~a" (name c)))))

(define-condition message-too-long (warning)
  ((%message :initarg :message :initform (error "Message required") :reader message)
   (%limit-form :initarg :limit :initform '*irc-message-limit* :reader limit-form))
  (:documentation "Condition signalled when an IRC message exceeds the byte limit set by *IRC-MESSAGE-LIMIT*")
  (:report (lambda (c s)
             (format s "Message ~s exceeds ~a (~a)" (message c) (limit-form c)
                     (let ((*package* (find-package "COLLEEN"))) (eval (limit-form c)))))))

(define-condition implicit-group-definition (style-warning)
  ((%group :initarg :group :reader group))
  (:documentation "Condition signalled when a group is implicitly defined for a grouped command.")
  (:report (lambda (c s)
             (format s "Implicitly creating new group command ~a" (group c)))))
