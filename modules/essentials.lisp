#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.essentials
  (:use :cl :colleen)
  (:shadowing-import-from :colleen :restart)
  (:shadow :shutdown))
(in-package :org.tymoonnext.colleen.mod.essentials)

(define-module essentials ()
  ())

(define-command reload essentials (event)
  (when (auth-p (nick event))
    (respond event (fstd-message event :config-reload "Reloading configuration."))
    (load-config)))

(define-command shutdown essentials (event)
  (when (auth-p (nick event))
    (broadcast (fstd-message event :shutdown))
    (loop for server being the hash-keys of *servers*
       do (disconnect server))))

(defcommand restart (user channel)
  (log:info "Restarting as per command...")
  (standard-message channel :restart)
  (error 'restart-error))

(defcommand irc (user channel (action &rest args))
  (handler-case 
      (let ((msg (format nil "~a~{ ~a~}" action args)))
        (log:info "Sending raw IRC message: ~a" msg)
        (trivial-irc:send-raw-message *con* msg))
    (error (err) (send-message channel "~a: Error during IRC command: ~a" user err))))

(defcommand error (user channel args)
  (error "Condition as per error function initiated by ~a in ~a with ~a" user channel args))

(defcommand echo (user channel args)
  (send-message channel (format nil "~{~a ~}" args)))

(defcommand time (user channel)
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
	(get-decoded-time)
    (declare (ignore dst-p))
    (send-message channel 
     (format nil "~a: It is now ~2,'0d:~2,'0d:~2,'0d, ~a the ~2,'0d/~2,'0d/~d (GMT~@d)"
             user hour minute second
             (nth day-of-week '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
             year month date (- tz)))))

