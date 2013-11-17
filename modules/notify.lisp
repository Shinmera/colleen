#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.notify
  (:use :cl :colleen :alexandria :local-time)
  (:shadowing-import-from :colleen :restart)
  (:shadow :load))
(in-package :org.tymoonnext.colleen.mod.notify)

(defparameter *timestamp-format* '(:long-weekday #\Space (:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(define-module notify ()
  ((%notes :initarg :notes :initform () :accessor notes)))

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

(defmethod start ((notify notify))
  (load notify))

(defmethod stop ((notify notify))
  (save notify))

(defmethod save ((notify notify))
  )

(defmethod load ((notify notify))
  )

(define-command (make-note notify) notify (recipient &rest message) ()
  (v:debug :notify "Creating new note by ~a for ~a" (nick event) recipient)
  (appendf (notes notify) 
           (list (make-instance 
                  'note 
                  :message (format NIL "~{~a~^ ~}" message) 
                  :sender (nick event) 
                  :nick recipient 
                  :channel (channel event) 
                  :server (string-upcase (name (server event)))
                  :timestamp (format-timestring NIL (now) :format *timestamp-format*))))
  (respond event "~a: Remembered. I will remind ~a when he/she/it next speaks." (nick event) recipient))

(define-handler notifier notify (privmsg-event event)
  (let ((newlist ()))
    (dolist (note (notes notify))
      (if (and (string= (nick event) (nick note))
               (string= (channel event) (channel note))
               (string= (string-upcase (name (server event))) (server note)))
          (progn (v:debug :notify "Delivering note ~a." note)
                 (respond event "~a: ~a wrote to you on ~a : ~a"
                          (nick note) (sender note) (timestamp note) (message note)))
          (push note newlist)))
    (setf (notes notify) (nreverse newlist))))
