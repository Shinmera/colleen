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
  ((%channel :initarg :channel :accessor channel)
   (%server :initarg :server :accessor server)))

(defmethod piping:print-self ((faucet syslog-faucet) stream)
  (format stream ">>~@[~a~]|~a/~a" (piping:name faucet) (server faucet) (channel faucet)))

(defmethod piping:pass ((faucet syslog-faucet) message)
  (let ((server (get-server (server faucet))))
    (when server
      (irc:privmsg (channel faucet) (format NIL "~a" message) :server server))))

(defmethod start ((syslog syslog))
  )

(defmethod stop ((syslog syslog))
  (loop for faucet being the hash-values of (faucets syslog)
        do (piping:disconnect-prev faucet))
  (setf (faucets syslog) (make-hash-table :test 'equalp)))

(define-group syslog :documentation "Change syslog faucet settings.")

(define-command (syslog log) (level category &rest message) (:authorization T :documentation "Send a new log message.")
  (assert (find level '("FATAL" "SEVERE" "ERROR" "WARN" "INFO" "DEBUG" "TRACE") :test #'string-equal)
          () "Level has to be one of (FATAL SEVERE ERROR WARN INFO DEBUG TRACE).")
  (v:log (find-symbol (string-upcase level) :keyword) (intern (string-upcase category) :keyword) "~{~a~^ ~}" message))

(define-command (syslog pipe) (name &optional (level "INFO") category channel server) (:authorization T :documentation "Start a new pipe to channel log messages through.")
  (assert (find level '("FATAL" "SEVERE" "ERROR" "WARN" "INFO" "DEBUG" "TRACE") :test #'string-equal)
          () "Level has to be one of (FATAL SEVERE ERROR WARN INFO DEBUG TRACE).")
  (let ((level (find-symbol (string-upcase level) :KEYWORD))
        (category (if category (intern (string-upcase category) :KEYWORD)))
        (channel (or channel (channel event)))
        (server (or (when server (find-symbol (string-upcase server) :KEYWORD)) (name (server event)))))
    (if level
        (progn
          (when (gethash name (faucets module))
            (respond event "Warning: Overwriting already existing faucet!")
            (piping:disconnect-prev (gethash name (faucets module))))
          (let ((faucet (make-instance 'syslog-faucet :name name :channel channel :server server)))
            (setf (gethash name (faucets module))
                  (v:attach-to level faucet :category category))
            (respond event "Faucet ~s added." name)))
        (respond event "WTF?"))))

(define-command (syslog stop) (name) (:authorization T :documentation "Stop and remove an existing pipe.")
  (let ((faucet (gethash name (faucets module))))
    (if faucet
        (progn
          (remhash name (faucets module))
          (piping:disconnect (piping:prev faucet) faucet)
          (respond event "Faucet ~s stopped." name))
        (respond event "No faucet named ~s found." name))))
