#|
This file is a part of Colleen
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.emoticons
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.emoticons)

(defvar *save-file* (merge-pathnames "emoticons-save.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module emoticon ()
  ((%db :initform (make-hash-table :test 'equal) :accessor db))
  (:documentation "Simple database for :emoticon:s."))

(defmethod start ((module emoticon))
  (with-open-file (stream *save-file* :if-does-not-exist NIL)
    (when stream
      (setf (db module) (yason:parse stream)))))

(defmethod stop ((module emoticon))
  (with-open-file (stream *save-file* :direction :output :if-does-not-exist :create :if-exists :supersede)
    (yason:encode (db module) stream)))

(define-handler (privmsg-event event) ()
  (let ((emoticon (gethash (string-downcase (message event)) (db module))))
    (when emoticon
      (respond event emoticon))))

(define-group emoticon :documentation "Manage :emoticon:s.")

(defun normalize-emoticon (string)
  (format NIL ":~a:" (string-downcase (string-trim '(#\:) string))))

(define-command (emoticon add) (name &rest emoticon) (:documentation "Add a new emoticon.")
  (setf name (normalize-emoticon name)
        emoticon (format NIL "~{~a~^ ~}" emoticon))
  (let ((existing (gethash name (db module))))
    (if existing
        (respond event "An emoticon with that name already exists: ~a" existing)
        (progn
          (setf (gethash name (db module)) emoticon)
          (respond event "Emoticon ~a -> ~a added." name emoticon)))))

(define-command (emoticon remove) (name) (:documentation "Remove an existing emoticon.")
  (setf name (normalize-emoticon name))
  (if (gethash name (db module))
      (progn
        (remhash name (db module))
        (respond event "Emoticon ~a removed." name))
      (respond event "No such emoticon exists.")))

(define-command (emoticon list) () (:documentation "List saved emoticon.")
  (respond event "~{~a~^, ~}" (loop for a being the hash-keys of (db module) collect a)))
