#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.chanblock
  (:nicknames :co-chanblock)
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.chanblock)

(define-module chanblock () ())

(define-handler (command-event event) (:priority :before :threaded NIL)
  (let ((channel (channel event))
        (server (string-downcase (name (server event)))))
    (when (find channel (uc:config-tree server) :test #'string-equal)
      (v:info :chanblock "Blocking on ~a/~a command: ~a" channel server event)
      (setf (cancelled event) T))))

(define-group chanblock :documentation "Block channels from being listened to for commands.")
(define-command (chanblock add) (&optional channel server) (:authorization T :documentation "Add a channel to the command block list.")
  (let ((channel (or channel (channel event)))
        (server (string-downcase (or server (name (server event))))))
    (pushnew channel (uc:config-tree server) :test #'string-equal)
    (respond event "Added ~a/~a to channel block list." server channel)))

(define-command (chanblock remove) (&optional channel server) (:authorization T :documentation "Remove a channel from the command block list.")
  (let ((channel (or channel (channel event)))
        (server (string-downcase (or server (name (server event))))))
    (setf (uc:config-tree server)
          (delete channel (uc:config-tree server) :test #'string-equal))
    (respond event "Removed ~a/~a from the channel block list." server channel)))
