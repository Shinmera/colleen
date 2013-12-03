#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(define-condition invalid-arguments (error)
  ((%command :initarg :command :reader command)
   (%argslist :initarg :argslist :reader argslist))
  (:documentation "Condition raised when a command is called with an invalid arguments structure.")
  (:report (lambda (c s)
             (format s "Invalid arguments for command ~a. Expected form: ~a" (command c) (argslist c)))))

(define-condition not-authorized (error)
  ((%event :initarg :event :reader event)))

(define-condition network-error (error) 
  ((%server :initarg :server :reader failed-server)))

(define-condition disconnect (network-error) ())

(define-condition connection-failed (network-error)
  ((%error :initarg :error :reader initial-error)))

(define-condition nickname-in-use (network-error)
  ((%nick :initarg :nick :reader nick)))

(define-condition ping-timeout (network-error) ())
