#|
 This file is a part of Colleen
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.syslog
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.syslog)

(define-module syslog ()
  ((%faucets :initform (make-hash-table :test 'equalp) :accessor faucets))
  (:documentation "Special faucet for verbose that relays log messages to a channel."))

(defclass syslog-faucet (piping:faucet)
  ((%channel :accessor channel)
   (%server :accessor server)))

(defmethod piping:pass ((faucet syslog-faucet) message)
  (let ((server (get-server (server faucet))))
    (when server
      (irc:privmsg (channel faucet) (format NIL message) :server server))))

(defmethod start ((syslog syslog))
  )

(defmethod stop ((syslog syslog))
  (loop for faucet being the hash-values of (faucets syslog)
        do (piping:disconnect-prev faucet))
  (setf (faucets syslog) (make-hash-table :test 'equalp)))

(define-group syslog :documentation "Change syslog faucet settings.")

(define-command (syslog pipe) (name &optional (level "INFO") category channel server) (:authorization T :documentation "")
  (assert (find level '("FATAL" "SEVERE" "ERROR" "WARN" "INFO" "DEBUG" "TRACE") :test #'string-equal)
          () "Level has to be one of (FATAL SEVERE ERROR WARN INFO DEBUG TRACE).")
  (let ((level (find-symbol (string-upcase level) :KEYWORD))
        (category (if category (intern (string-upcase category) :KEYWORD)))
        (channel (or channel (channel event)))
        (server (or (find-symbol (string-upcase server) :KEYWORD) (name (server event)))))
    (when (gethash name (faucets module))
      (respond event "Warning: Overwriting already existing faucet!")
      (piping:disconnect-prev (gethash name (faucets module))))
    (let ((faucet (make-instance 'syslog-faucet :channel channel :server server)))
      (setf (gethash name (faucets module))
            (v:attach-to level faucet :category category))
      (respond event "Faucet ~s added." name))))

(define-command (syslog stop) (name) (:authorization T :documentation "")
  (let ((faucet (gethash name (faucets module))))
    (if faucet
        (progn
          (remhash name (faucets module))
          (piping:disconnect-prev faucet)
          (respond event "Faucet ~s stopped." name))
        (respond event "No faucet named ~s found." name))))
