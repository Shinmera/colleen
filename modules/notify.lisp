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
(defparameter *save-file* (merge-pathnames "notify-save.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

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
  (with-open-file (stream *save-file* :direction :output :if-does-not-exist :create :if-exists :supersede)
    (let ((newlist ()))
      (dolist (note (notes notify))
        (let ((table (make-hash-table :test 'equal)))
          (setf (gethash "message" table) (message note))
          (setf (gethash "sender" table) (sender note))
          (setf (gethash "nick" table) (nick note))
          (setf (gethash "channel" table) (channel note))
          (setf (gethash "server" table) (server note))
          (setf (gethash "timestamp" table) (timestamp note))
          (push table newlist)))
      (yason:encode (nreverse newlist) stream)
      (v:info :notify "Saved ~d notes." (length newlist)))))

(defmethod load ((notify notify))
  (with-open-file (stream *save-file* :if-does-not-exist NIL)
    (when stream
      (let ((notes (yason:parse stream))
            (newlist ()))
        (dolist (note notes)
          (push (make-instance
                 'note
                 :message (gethash "message" note)
                 :sender (gethash "sender" note)
                 :nick (gethash "nick" note)
                 :channel (gethash "channel" note)
                 :server (gethash "server" note)
                 :timestamp (gethash "timestamp" note))
                newlist))
        (setf (notes notify) (nreverse newlist))
        (v:info :notify "Loaded ~d notes." (length newlist))))))

(define-command notify (recipient &rest message) (:modulevar notify)
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

(define-handler (privmsg-event event) (:modulevar notify)
  (let ((newlist ()))
    (dolist (note (notes notify))
      (if (and (string-equal (nick event) (nick note))
               (string= (channel event) (channel note))
               (string= (string-upcase (name (server event))) (server note)))
          (progn (v:debug :notify "Delivering note ~a." note)
                 (respond event "~a: ~a wrote to you on ~a : ~a"
                          (nick note) (sender note) (timestamp note) (message note)))
          (push note newlist)))
    (setf (notes notify) (nreverse newlist))))
