#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.notify
  (:use :cl :colleen :events :alexandria :local-time)
  (:shadow :load))
(in-package :org.tymoonnext.colleen.mod.notify)

(defparameter *timestamp-format* '(:long-weekday #\Space (:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(define-module notify () () (:documentation "Simple module to send out notifications."))

(defclass note ()
  ((%server :initarg :server :initform NIL :accessor server)
   (%channel :initarg :channel :initform NIL :accessor channel)
   (%nickname :initarg :nick :initform NIL :accessor nick)
   (%sender :initarg :sender :initform NIL :accessor sender)
   (%timestamp :initarg :timestamp :initform NIL :accessor timestamp)
   (%message :initarg :message :initform NIL :accessor message)))

(defmethod print-object ((note note) stream)
  (print-unreadable-object (note stream :type T)
    (format stream "~a ~a ~a ~a -> ~a" (timestamp note) (server note) (channel note) (sender note) (nick note))))

(uc:define-serializer (note note T)
  (make-array 6 :initial-contents (list (server note) (channel note) (nick note) (sender note) (timestamp note) (message note))))

(uc:define-deserializer (note array T)
  (make-instance 'note
                 :server (aref array 0)
                 :channel (aref array 1)
                 :nick (aref array 2)
                 :sender (aref array 3)
                 :timestamp (aref array 4)
                 :message (aref array 5)))

(define-command notify (recipient &rest message) (:modulevar notify)
  (if (string-equal recipient (nick (server event)))
      (respond event "~a: I'm right here." (nick event))
      (progn
        (v:debug :notify "Creating new note by ~a for ~a" (nick event) recipient)
        (push (make-instance 
               'note 
               :message (format NIL "~{~a~^ ~}" message) 
               :sender (nick event) 
               :nick recipient 
               :channel (channel event) 
               :server (string-upcase (name (server event)))
               :timestamp (format-timestring NIL (now) :format *timestamp-format*))
              (uc:config-tree :notes))
        (respond event "~a: Remembered. I will remind ~a when he/she/it next speaks." (nick event) recipient))))

(define-handler (privmsg-event event) (:modulevar notify)
  (let ((newlist ()))
    (dolist (note (uc:config-tree :notes))
      (if (and (string-equal (nick event) (nick note))
               (string= (channel event) (channel note))
               (string= (string-upcase (name (server event))) (server note)))
          (progn (v:debug :notify "Delivering note ~a." note)
                 (respond event "~a: ~a wrote to you on ~a : ~a"
                          (nick note) (sender note) (timestamp note) (message note)))
          (push note newlist)))
    (setf (uc:config-tree :notes) (nreverse newlist))))
