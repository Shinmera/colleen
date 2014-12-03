#|
 This file is a part of Colleen
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.emoticons
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.emoticons)

(define-module emoticon () ()
  (:documentation "Simple database for :emoticon:s."))

(define-handler (privmsg-event event) ()
  (cl-ppcre:register-groups-bind (emoticon NIL) ("(:([^\\s]+?):)" (message event))
    (let ((emoticon (uc:config-tree (string-downcase emoticon))))
      (when emoticon
        (respond event emoticon)))))

(define-group emoticon :documentation "Manage :emoticon:s.")

(defun normalize-emoticon (string)
  (format NIL ":~a:" (string-downcase (string-trim '(#\:) string))))

(define-command (emoticon add) (name &rest emoticon) (:documentation "Add a new emoticon.")
  (setf name (normalize-emoticon name)
        emoticon (format NIL "~{~a~^ ~}" emoticon))
  (let ((existing (uc:config-tree name)))
    (if existing
        (respond event "An emoticon with that name already exists: ~a" existing)
        (progn
          (setf (uc:config-tree name) emoticon)
          (respond event "Emoticon ~a -> ~a added." name emoticon)))))

(define-command (emoticon remove) (name) (:documentation "Remove an existing emoticon.")
  (setf name (normalize-emoticon name))
  (if (uc:config-tree name)
      (progn
        (remhash name (storage module))
        (respond event "Emoticon ~a removed." name))
      (respond event "No such emoticon exists.")))

(define-command (emoticon list) () (:documentation "List saved emoticon.")
  (respond event "~{~a~^, ~}" (loop for a being the hash-keys of (storage module) collect a)))
