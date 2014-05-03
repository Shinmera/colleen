#|
 This file is a part of Colleen
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.counter
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.counter)

(define-module counter ()
  (:documentation "Count objects."))

(define-handler (privmsg-event event) ()
  (loop for name being the hash-keys of uc:*config*
        for (regexp count active-in) being the hash-values of uc:*config*
        do (when (and (find (format NIL "~a/~a" (name (server event)) (channel event)) active-in :test #'string-equal)
                      (cl-ppcre:scan regexp (string-downcase (message event))))
             (setf (second (config-tree name))
                   (1+ count))
             (respond event "~a counter: ~d" name (1+ count)))))

(define-group counter :documentation "Checks messages for a regexp and ups a counter if matched.")

(define-command (counter count) (name regexp &optional channel server) (:authorization T :documentation "Add a new regexp to count.")
  (when (config-tree name)
    (respond event "Warning: Overwriting existing counter!"))
  (let ((chanident (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
    (setf (config-tree name) (list regexp 0 (list chanident)))
    (respond event "Now counting for ~s on ~a." name chanident)))

(define-command (counter reset) (name) (:authorization T :documentation "Reset a count.")
  (if (config-tree name)
      (setf (second (config-tree name)) 0)
      (respond event "No counter called ~s found." name)))

(define-command (counter activate) (name &optional channel server) (:authorization T :documentation "Activate a counter for a channel.")
  (if (config-tree name)
      (let ((chanident (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
        (pushnew chanident (third (config-tree name)) :test #'string-equal)
        (respond event "Now counting for ~s on ~a." name chanident))
      (respond event "No counter called ~s found." name)))

(define-command (counter deactivate) (name &optional channel server) (:authorization T :documentation "Deactivate a counter for a channel.")
  (if (config-tree name)
      (let ((chanident (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
        (setf (third (config-tree name))
              (delete chanident (third (config-tree name)) :test #'string-equal))
        (respond event "Stopped counting for ~s on ~a." name chanident))
      (respond event "No counter called ~s found." name)))

(define-command (counter remove) (name) (:authorization T :documentation "Remove a counter completely.")
  (if (config-tree name)
      (progn
        (remhash name uc:*config*)
        (respond event "Removed counter ~s." name))
      (respond event "No counter called ~s found." name)))
