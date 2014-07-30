#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.notify
  (:use :cl :colleen :events :local-time)
  (:nicknames :co-notify))
(in-package :org.tymoonnext.colleen.mod.notify)

(defparameter *timestamp-format* '(:long-weekday #\Space (:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(define-module notify () () (:documentation "Simple module to send out notifications."))

(defclass note ()
  ((%server :initarg :server :initform NIL :accessor server)
   (%channel :initarg :channel :initform NIL :accessor channel)
   (%nickname :initarg :nick :initform NIL :accessor nick)
   (%sender :initarg :sender :initform NIL :accessor sender)
   (%timestamp :initarg :timestamp :initform NIL :accessor timestamp)
   (%message :initarg :message :initform NIL :accessor message)
   (%trigger :initarg :trigger :initform :message :accessor trigger)))

(defmethod print-object ((note note) stream)
  (print-unreadable-object (note stream :type T)
    (format stream "~a ~a ~a ~a -> ~a" (timestamp note) (server note) (channel note) (sender note) (nick note))))

(uc:define-serializer (note note T)
  (make-array 7 :initial-contents (list (server note) (channel note) (nick note) (sender note) (timestamp note) (message note) (trigger note))))

(uc:define-deserializer (note array T)
  (make-instance 'note
                 :server (aref array 0)
                 :channel (aref array 1)
                 :nick (aref array 2)
                 :sender (aref array 3)
                 :timestamp (aref array 4)
                 :message (aref array 5)
                 :trigger (aref array 6)))

(defmethod start ((notify notify))
  (with-module-storage (notify)
    (dolist (note (uc:config-tree :notes))
      (when (integerp (nick note))
        (schedule-timer 'notify :once (nick note) :arguments (list note))))))

(defun make-note (event recipient message &rest overrides)
  (let ((args (list :message (format NIL "~{~a~^ ~}" message)
                    :nick recipient
                    :sender (nick event)
                    :channel (channel event)
                    :server (string-upcase (name (server event)))
                    :timestamp (format-timestring NIL (now) :format *timestamp-format*)
                    :trigger :message)))
    (loop for (key val) on overrides by #'cddr
          do (setf (getf args key) val))
    (apply #'make-instance 'note args)))

(define-command |notify @join| (recipient &rest message) (:documentation "Notify MESSAGE to RECIPIENT when they next join this channel.")
  (v:debug :notify "Creating new note by ~a for ~a" (nick event) recipient)
  (push (make-note event recipient message :trigger :join)
        (uc:config-tree :notes))
  (respond event "~a: Remembered. I will remind ~a when he/she/it next joins." (nick event) recipient))

(define-command |notify @any| (&rest message) (:documentation "Say MESSAGE when any lines are spoken in the chatroom.")
  (v:debug :notify "Creating new note for anyone by ~a" (nick event))
  (push (make-note event :any message)
        (uc:config-tree :notes))
  (respond event "~a: Remembered. I will repost this whenever someone else next next talks." (nick event)))

(define-command |notify @date| (date &rest message) (:documentation "Notify MESSAGE to this channel once DATE is reached. DATE should be Y.M.DTh:m:s")
  (handler-case
      (let ((timestamp (timestamp-to-universal
                        (parse-timestring (format NIL "~a+~a:00" date
                                                   (/ (nth-value 9 (local-time:decode-timestamp (local-time:now))) 60 60))))))
        (v:debug :notify "Creating new note by ~a scheduled for ~a" (nick event) timestamp)
        (let ((note (make-note event timestamp message :trigger :date)))
          (push note (uc:config-tree :notes))
          (schedule-timer 'notify :once timestamp :arguments (list note))
          (respond event "~a: Remembered. I will repost this on ~a." (nick event) date)))
    (local-time::invalid-timestring (err)
      (declare (ignore err))
      (respond event "Invalid date. Should be an RFC3339 compatible string like so: 2014-05-30T16:22:43"))))

(define-command |notify @in| (time &rest message) (:documentation "Notify MESSAGE to this channel once TIME has passed. TIME should be h:m:s with the seconds being optional.")
  (handler-case
      (let* ((date (split-sequence:split-sequence #\: time))
             (timestamp (case (length date)
                          ((0 1) (error ""))
                          ((2 3) (destructuring-bind (h m &optional (s "0")) date
                                   (+ (get-universal-time)
                                      (* (parse-integer s) 1) (* (parse-integer m) 60) (* (parse-integer h) 60 60))))
                          (T (error "")))))
        (v:debug :notify "Creating new note by ~a scheduled for ~a" (nick event) timestamp)
        (let ((note (make-note event timestamp message :trigger :time)))
          (push note (uc:config-tree :notes))
          (schedule-timer 'notify :once timestamp :arguments (list note))
          (respond event "~a: Remembered. I will repost this in ~a." (nick event) time)))
    (error (err)
      (declare (ignore err))
      (respond event "Invalid date. Should be an RFC3339 compatible string like so: 16:22:43"))))

(define-command notify (recipient &rest message) (:documentation "Notify MESSAGE to RECIPIENT when they next speak in this channel.")
  (cond ((string-equal recipient (nick (server event)))
         (respond event "~a: I'm right here." (nick event)))
        ((string-equal recipient (nick event))
         (respond event "~a: Are you feeling lonely?" (nick event)))
        (T
         (v:debug :notify "Creating new note by ~a for ~a" (nick event) recipient)
         (push (make-note event recipient message)
               (uc:config-tree :notes))
         (respond event "~a: Remembered. I will remind ~a when he/she/it next speaks." (nick event) recipient))))

(define-handler (privmsg-event event) (:documentation "Checks for notifications to deliver to the speaker.")
  (let ((newlist ()))
    (dolist (note (uc:config-tree :notes))
      (cond
        ((and (eql (trigger note) :message)
              (string= (channel note) (channel event))
              (string= (server note) (string-upcase (name (server event)))))
         (cond
           ((and (stringp (nick note))
                 (string-equal (nick note) (nick event)))
            (v:debug :notify "Delivering note ~a." note)
            (respond event "~a: ~a wrote to you on ~a : ~a"
                     (nick note) (sender note) (timestamp note) (message note)))
               
           ((and (keywordp (nick note))
                 (eql (nick note) :any)
                 (string-not-equal (nick event) (sender note)))
            (v:debug :notify "Delivering note ~a." note)
            (respond event "~a wrote on ~a: ~a"
                     (nick note) (timestamp note) (message note)))))
        
        (t (push note newlist)))
      (setf (uc:config-tree :notes) (nreverse newlist)))))

(define-handler (join-event event) (:documentation "Checks for notifications to deliver to the joiner.")
  (let ((newlist ()))
    (dolist (note (uc:config-tree :notes))
      (if (and (stringp (nick note))
               (string-equal (nick note) (nick event))
               (string= (channel note) (channel event))
               (string= (server note) (string-upcase (name (server event))))
               (eql (trigger note) :join))
          (progn (v:debug :notify "Delivering note ~a." note)
                 (respond event "~a: ~a wrote to you on ~a : ~a"
                          (nick note) (sender note) (timestamp note) (message note)))
          (push note newlist)))
    (setf (uc:config-tree :notes) (nreverse newlist))))

(define-timer notify (note) (:documentation "Used for timed notifications.")
  (v:debug :notify "Delivering note ~a." note)
  (irc:privmsg (channel note) (format NIL "~a wrote on ~a : ~a" (sender note) (timestamp note) (message note))
               :server (get-server (find-symbol (server note) "KEYWORD")))
  (setf (uc:config-tree :notes)
        (delete note (uc:config-tree :notes))))
