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
   (%argslist :initarg :argslist :reader argslist))
  (:documentation "Condition raised when a command is called with an invalid arguments structure.")
  (:report (lambda (c s)
             (format s "Invalid arguments for command ~a. Expected form: ~a" (command c) (argslist c)))))

(define-condition not-authorized (module-error)
  ((%event :initarg :event :reader event))
  (:documentation "Condition raised when a user tried to execute a command he isn't authorized for."))

(define-condition network-error (error) 
  ((%server :initarg :server :reader failed-server))
  (:documentation "Condition superclass for network releated errors."))

(define-condition disconnect (network-error) ()
  (:documentation "Condition to disconnect and leave the handler loops."))

(define-condition connection-failed (network-error)
  ((%error :initarg :error :reader initial-error))
  (:documentation "Condition when the connection attempt failed for some reason. The original error is included."))

(define-condition ping-timeout (network-error) ()
  (:documentation "Condition signalled when a ping-timeout is noticed."))

(define-condition module-stop (condition) ()
  (:documentation "Condition signalled when a thread is interrupted due to a module-stop."))
