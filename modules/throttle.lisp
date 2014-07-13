#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.throttle
  (:nicknames :co-throttle)
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.throttle)

(defvar *throttles* (make-hash-table :test 'equalp))
(defvar *default-throttle* 5)

(define-module throttle () ()
  (:documentation "Throttles when a user sends commands too frequently."))

(defmethod start ((throttle throttle))
  (with-module-storage (throttle)
    (unless (uc:config-tree :amount)
      (setf (uc:config-tree :amount) *default-throttle*))))

(define-handler (command-event event) (:priority :before :threaded NIL)
  (let* ((real-nick (format NIL "~a/~a" (name (server event)) (nick event)))
         (throttle-val (gethash real-nick *throttles*)))
    (setf (gethash real-nick *throttles*) (get-universal-time))
    (when (and throttle-val
               (< (- (get-universal-time) throttle-val) (uc:config-tree :amount))
               (not (auth-p (nick event))))
      (setf (cancelled event) T)
      (v:warn :throttle "Throttling ~a by ~a" event real-nick)
      (respond event "~a" (fstd-message event :throttle "~a: Calm down for ~r second~:p, jeez." (nick event) (uc:config-tree :amount))))))

(define-command set-throttle (seconds) (:documentation "Set the throttling delay in seconds." :authorization T)
  (setf (uc:config-tree :amount) (parse-integer seconds))
  (respond event "Throttle changed to ~a" seconds))
