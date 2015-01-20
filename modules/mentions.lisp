#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.mentions
  (:nicknames #:co-mentions)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.mentions)

(define-module mentions () ())

(defmethod start ((mentions mentions))
  (with-module-storage (mentions)
    (macrolet ((set-new (place value)
                 `(unless (uc:config-tree ,place)
                    (setf (uc:config-tree ,place) ,value))))
      (set-new :mentions (make-hash-table :test 'equalp))
      (set-new :delay (* 60 60))
      (set-new :active ()))))

(defun id (event &key (server (name (server event))) (channel (channel event)) (nick (nick event)))
  (format NIL "~a/~a/~a"
          server
          channel
          ;; Strip common "dupe nick" pre/suffixes
          (string-trim "`-_\\" nick)))

(defconstant +UNIX-EPOCH-DIFFERENCE+ (encode-universal-time 0 0 0 1 1 1970 0))
(defun get-unix-time (&optional (time (get-universal-time)))
  (- time +UNIX-EPOCH-DIFFERENCE+))

(defun format-universal-time (ut)
  (local-time:format-timestring
   NIL (local-time:universal-to-timestamp ut)
   :format '((:year 4) "." :month "." :day ", " :long-weekday " " (:hour 2) ":" (:min 2) ":" (:sec 2) " UTC")
   :timezone local-time:+utc-zone+))

(defun external-chatlog-link (server channel universal-time)
  (when (and (find-package :co-chatlog-pg)
             (funcall (find-symbol "ACTIVE-P" :co-chatlog-pg) server channel))
    (funcall (find-symbol "EXTERNAL-ADDRESS" :co-chatlog-pg) server channel (get-unix-time universal-time))))

(define-handler (privmsg-event event) ()
  (when (find (id event :nick "") (uc:config-tree :active) :test #'string-equal)
    ;; Record mention
    (cl-ppcre:register-groups-bind (name) ("^([a-zA-Z0-9_\\-`\\[\\]\\|]+)[:,]" (message event))
      (when (and (find name (users (channel event)) :test #'string-equal)
                 (not (string-equal name (nick (server event)))))
        (let* ((id (id event :nick name))
               (mentions (uc:config-tree :mentions id)))
          (cond
            (mentions
             (v:info :mentions "Adding ~a to mention info for ~a" (nick event) id)
             (incf (second mentions))
             (pushnew (nick event) (third mentions) :test #'string-equal))
            (T
             (v:info :mentions "Creating new mention info for ~a by ~a" id (nick event))
             (setf (uc:config-tree :mentions id)
                   (list (get-universal-time)
                         1
                         (list (nick event)))))))))
    ;; Relay mention
    (let ((mentions (uc:config-tree :mentions (id event)))) 
      (when mentions
        (cond ((and
                ;; Delay can't be overstepped.
                (< (uc:config-tree :delay) (- (get-universal-time) (first mentions)))
                ;; The user mustn't be responding to one of his mentioners.
                (loop for mentioner in (third mentions)
                      never (search mentioner (message event) :test #'char-equal)))
               ;; Ok, we hope they really didn't notice.
               (v:info :mentions "Relaying mention info for ~a" (id event))
               (respond event "~a: You have been mentioned ~d time~:p by ~{~a~#[~;, and ~:;, ~]~} first at ~a~@[ ~a~]"
                        (nick event) (second mentions) (third mentions) (format-universal-time (first mentions))
                        (external-chatlog-link (name (server event)) (channel event) (first mentions))))
              (T
               (v:info :mentions "Clearing mention info for ~a" (id event))))
        (setf (uc:config-tree :mentions (id event)) ())))))

(define-group mentions :documentation "Change mention reminder settings.")

(define-command (mentions activate) (&optional channel server) (:authorization T :documentation "Activate mention reminding.")
  (let ((channel (or channel (channel event)))
        (server (or server (name (server event)))))
    (pushnew (id event :channel channel :server server :nick "")
             (uc:config-tree :active) :test #'string-equal)
    (respond event "Mention reminding activated for ~a/~a." server channel)))

(define-command (mentions deactivate) (&optional channel server) (:authorization T :documentation "Deactivate mention reminding.")
  (let ((channel (or channel (channel event)))
        (server (or server (name (server event)))))
    (setf (uc:config-tree :active)
          (delete (id event :channel channel :server server :nick "")
                  (uc:config-tree :active) :test #'string-equal))
    (respond event "Mention reminding deactivated for ~a/~a." server channel)))

(define-command (mentions delay) (&optional new-delay) (:authorization T :documentation "Show or set the necessary mention delay in seconds.")
  (when new-delay
    (setf (uc:config-tree :delay) (parse-integer new-delay)))
  (respond event "Necessary mention delay is ~a second~:p." (uc:config-tree :delay)))

(define-command (mentions clear) (id) (:authorization T :documentation "Clear mentions for a certain ID (server/channel/nick).")
  (let ((mentions (uc:config-tree :mentions id)))
    (cond (mentions
           (setf (uc:config-tree :mentions id) ())
           (respond event "~a had ~d mention~:p by ~{~a~#[~;, and ~:;, ~]~} first at ~a"
                    id (second mentions) (third mentions) (format-universal-time (first mentions))))
          (T
           (respond event "No mentions to clear." id)))))
