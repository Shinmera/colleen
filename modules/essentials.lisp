#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.essentials
  (:use :cl :colleen :local-time)
  (:shadowing-import-from :colleen :restart)
  (:shadow :shutdown))
(in-package :org.tymoonnext.colleen.mod.essentials)

(define-module essentials ()
  ())

(define-command reload essentials () (:authorization T)
  (when (auth-p (nick event))
    (respond event (fstd-message event :config-reload "Reloading configuration."))
    (load-config)))

(define-command shutdown essentials () (:authorization T)
  (when (auth-p (nick event))
    (irc:broadcast (fstd-message event :shutdown))
    (loop for server being the hash-keys of *servers*
       do (disconnect server))))

(define-command irc essentials (action &rest args) (:authorization T)
  (handler-case 
      (let ((msg (format nil "~a~{ ~a~}" action args)))
        (v:info :essentials "Sending raw IRC message: ~a" msg)
        (irc:send-raw msg))
    (error (err) (respond event "~a: Error during IRC command: ~a" (nick event) err))))

(define-command (make-error error) essentials () ()
  (error "Condition as per error function initiated by ~a in ~a." (nick event) (channel event)))

(define-command echo essentials (&rest args) ()
  (respond event "~{~a ~}" args))

(define-command (send-time time) essentials () ()
  (respond event "~a: It is now ~a" (nick event)
           (format-timestring NIL (now) :format 
                              '((:year 4) #\. :month #\. :day #\, #\Space :long-weekday #\Space :hour #\: :min #\: :sec #\Space #\( :timezone #\/ #\G #\M #\T :gmt-offset #\)))))

