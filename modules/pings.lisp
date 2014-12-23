#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.pings
  (:nicknames #:co-pings)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.pings)

(define-module pings () ())

(defmethod start ((pings pings))
  (with-module-storage (pings)
    (macrolet ((set-new (place value)
                 `(unless (uc:config-tree ,place)
                    (setf (uc:config-tree ,place) ,value))))
      (set-new :pings (make-hash-table :test 'equalp))
      (set-new :delay (* 60 60))
      (set-new :active ()))))

(defun id (event &key (server (name (server event))) (channel (channel event)) (nick (nick event)))
  (format NIL "~a/~a/~a"
          server
          channel
          ;; Strip common "dupe nick" pre/suffixes
          (string-trim "`-_\\" nick)))

(defun format-universal-time (ut)
  (format NIL "~:@{~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~}"
          (subseq (nreverse (multiple-value-list (decode-universal-time ut))) 3)))

(define-handler (privmsg-event event) ()
  (when (find (id event :nick "") (uc:config-tree :active) :test #'string-equal)
    ;; Record ping
    (cl-ppcre:register-groups-bind (name) ("^([a-zA-Z0-9_\\-`\\[\\]\\|]+)[:,]" (message event))
      (when (and (find name (users (channel event)) :test #'string-equal)
                 (not (string-equal name (nick (server event)))))
        (let* ((id (id event :nick name))
               (pings (uc:config-tree :pings id)))
          (cond
            (pings
             (v:info :pings "Adding ~a to ping info for ~a" (nick event) id)
             (incf (second pings))
             (pushnew (nick event) (third pings) :test #'string-equal))
            (T
             (v:info :pings "Creating new ping info for ~a by ~a" id (nick event))
             (setf (uc:config-tree :pings id)
                   (list (get-universal-time)
                         1
                         (list (nick event)))))))))
    ;; Relay ping
    (let ((pings (uc:config-tree :pings (id event)))) 
      (when pings
        (cond ((and
                ;; Delay can't be overstepped.
                (< (uc:config-tree :delay) (- (get-universal-time) (first pings)))
                ;; The user mustn't be responding to one of his pingers.
                (loop for pinger in (third pings)
                      never (search pinger (message event) :test #'char-equal)))
               ;; Ok, we hope they really didn't notice.
               (v:info :pings "Relaying ping info for ~a" (id event))
               (respond event "~a: You have been pinged ~d time~:p by ~{~a~#[~;, and ~:;, ~]~} first at ~a"
                        (nick event) (second pings) (third pings) (format-universal-time (first pings))))
              (T
               (v:info :pings "Clearing ping info for ~a" (id event))
               (setf (uc:config-tree :pings (id event)) ())))))))

(define-group pings :documentation "Change ping reminder settings.")

(define-command (pings activate) (&optional channel server) (:authorization T :documentation "Activate ping reminding.")
  (let ((channel (or channel (channel event)))
        (server (or server (name (server event)))))
    (pushnew (id event :channel channel :server server :nick "")
             (uc:config-tree :active) :test #'string-equal)
    (respond event "Ping reminding activated for ~a/~a." server channel)))

(define-command (pings deactivate) (&optional channel server) (:authorization T :documentation "Deactivate ping reminding.")
  (let ((channel (or channel (channel event)))
        (server (or server (name (server event)))))
    (setf (uc:config-tree :active)
          (delete (id event :channel channel :server server :nick "")
                  (uc:config-tree :active) :test #'string-equal))
    (respond event "Ping reminding deactivated for ~a/~a." server channel)))

(define-command (pings delay) (&optional new-delay) (:authorization T :documentation "Show or set the necessary ping delay in seconds.")
  (when new-delay
    (setf (uc:config-tree :delay) (parse-integer new-delay)))
  (respond event "Necessary ping delay is ~a second~:p." (uc:config-tree :delay)))

(define-command (pings clear) (id) (:authorization T :documentation "Clear pings for a certain ID (server/channel/nick).")
  (let ((pings (uc:config-tree :pings id)))
    (cond (pings
           (setf (uc:config-tree :pings id) ())
           (respond event "~a had ~d ping~:p by ~{~a~#[~;, and ~:;, ~]~} first at ~a"
                    id (second pings) (third pings) (format-universal-time (first pings))))
          (T
           (respond event "No pings to clear." id)))))
