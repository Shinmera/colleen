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

(define-condition disconnect (error) ())

(define-condition connection-failed (error)
  ((%server :initarg :server :reader failed-server)
   (%error :initarg :error :reader initial-error)))

(define-condition nickname-in-use (error)
  ((%server :initarg :server :reader failed-server)
   (%nick :initarg :nick :reader nick)))

(define-condition not-authorized (error)
  ((%event :initarg :event :reader event)))
