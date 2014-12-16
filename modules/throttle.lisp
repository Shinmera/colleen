#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.throttle
  (:nicknames #:co-throttle)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.throttle)

(defvar *throttles* (make-hash-table :test 'equalp))
(defvar *throttle-time* 8)
(defvar *throttle-count* 5)

(define-module throttle () ()
  (:documentation "Throttles when a user sends commands too frequently."))

(defmethod start ((throttle throttle))
  (with-module-storage (throttle)
    (unless (uc:config-tree :time)
      (setf (uc:config-tree :time) *throttle-time*))
    (unless (uc:config-tree :count)
      (setf (uc:config-tree :count) *throttle-count*))))

(define-handler (command-event event) (:priority :before :threaded NIL)
  (let* ((real-nick (format NIL "~a/~a" (name (server event)) (nick event))))
    (destructuring-bind (count time) (or (gethash real-nick *throttles*)
                                         (list 0 0))
      (cond
        ((auth-p (nick event))
         ;; passthrough
         )
        ((< 0 count)
         (decf (first (gethash real-nick *throttles*))))
        ((< (uc:config-tree :time) (- (get-universal-time) time))
         (setf (gethash real-nick *throttles*) (list (uc:config-tree :count) (get-universal-time))))
        (T
         (setf (cancelled event) T)
         (v:warn :throttle "Throttling ~a by ~a" event real-nick)
         (respond event "~a" (fstd-message event :throttle "~a: You are being way too noisy, ~r message~:p in ~r second~:p is too much."
                                           (nick event) (uc:config-tree :count) (- (get-universal-time) time))))))))

(define-command set-throttle (seconds amount) (:documentation "Set the throttling delay" :authorization T)
  (setf (uc:config-tree :time) (parse-integer seconds))
  (setf (uc:config-tree :count) (parse-integer amount))
  (respond event "Throttle changed to ~a messages per ~a seconds." amount seconds))
