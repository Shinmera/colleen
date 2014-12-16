#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.syslog
  (:nicknames #:co-syslog)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.syslog)

(define-module syslog ()
  ((%faucets :initform (make-hash-table :test 'equalp) :accessor faucets))
  (:documentation "Special faucet for verbose that relays log messages to a channel."))

(defclass syslog-faucet (piping:faucet)
  ((%channel :initarg :channel :accessor channel)
   (%server :initarg :server :accessor server)))

(defmethod piping:pass ((faucet syslog-faucet) message)
  (let ((server (get-server (server faucet))))
    (when server
      (irc:privmsg (channel faucet) (format NIL "~a" message) :server server)))
  message)

(defmethod stop ((syslog syslog))
  (loop for name being the hash-keys of (faucets syslog)
        do (piping:remove-segment v:*global-controller* name))
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
        (server (or (when server (find-symbol (string-upcase server) :KEYWORD)) (name (server event))))
        (name (intern (string-upcase name))))
    (if level
        (progn
          (when (gethash name (faucets module))
            (respond event "Warning: Overwriting already existing faucet!")
            (piping:remove-segment v:*global-controller* name))
          (let ((faucet (make-instance 'syslog-faucet :name name :channel channel :server server)))
            (setf (gethash name (faucets module)) faucet)
            (let ((position (piping:add-segment v:*global-controller* faucet)))
              (piping:set-name v:*global-controller* (list position) name))
            (respond event "Faucet ~s added." name)))
        (respond event "WTF?"))))

(define-command (syslog stop) (name) (:authorization T :documentation "Stop and remove an existing pipe.")
  (let* ((name (find-symbol (string-upcase name)))
         (faucet (gethash name (faucets module))))
    (if faucet
        (progn
          (remhash name (faucets module))
          (piping:remove-segment v:*global-controller* name)
          (respond event "Faucet ~s stopped." name))
        (respond event "No faucet named ~s found." name))))
