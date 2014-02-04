#|
This file is a part of Colleen
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.counter
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.counter)

(defvar *save-file* (merge-pathnames "counter-save.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module counter ()
    ((counter :initform (make-hash-table :test 'equal) :accessor counter))
  (:documentation "Count objects."))

(defmethod start ((counter counter))
  (%load counter))

(defmethod stop ((counter counter))
  (%save counter))

(defmethod %load ((module counter))
  (with-open-file (stream *save-file* :if-does-not-exist NIL)
    (when stream
      (let ((counter (yason:parse stream)))
        (loop for k being the hash-keys of counter
              for v being the hash-values of counter
              do (setf (gethash k (counter module)) v))))))

(defmethod %save ((module counter))
  (with-open-file (stream *save-file* :direction :output :if-does-not-exist :create :if-exists :supersede)
    (yason:encode (counter module) stream)))

(define-handler (privmsg-event event) ()
  (loop for name being the hash-keys of (counter module)
        for (regexp count active-in) being the hash-values of (counter module)
        do (when (and (find (format NIL "~a/~a" (name (server event)) (channel event)) active-in :test #'string-equal)
                      (cl-ppcre:scan regexp (string-downcase (message event))))
             (setf (second (gethash name (counter module)))
                   (1+ count))
             (respond event "~a counter: ~d" name (1+ count)))))

(define-group counter :documentation "Checks messages for a regexp and ups a counter if matched.")

(define-command (counter count) (name regexp &optional channel server) (:authorization T :documentation "Add a new regexp to count.")
  (when (gethash name (counter module))
    (respond event "Warning: Overwriting existing counter!"))
  (let ((chanident (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
    (setf (gethash name (counter module)) (list regexp 0 (list chanident)))
    (respond event "Now counting for ~s on ~a." name chanident)))

(define-command (counter reset) (name) (:authorization T :documentation "Reset a count.")
  (if (gethash name (counter module))
      (setf (second (gethash name (counter module))) 0)
      (respond event "No counter called ~s found." name)))

(define-command (counter activate) (name &optional channel server) (:authorization T :documentation "Activate a counter for a channel.")
  (if (gethash name (counter module))
      (let ((chanident (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
        (pushnew chanident (third (gethash name (counter module))) :test #'string-equal)
        (respond event "Now counting for ~s on ~a." name chanident))
      (respond event "No counter called ~s found." name)))

(define-command (counter deactivate) (name &optional channel server) (:authorization T :documentation "Deactivate a counter for a channel.")
  (if (gethash name (counter module))
      (let ((chanident (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
        (setf (third (gethash name (counter module)))
              (delete chanident (third (gethash name (counter module))) :test #'string-equal))
        (respond event "Stopped counting for ~s on ~a." name chanident))
      (respond event "No counter called ~s found." name)))

(define-command (counter remove) (name) (:authorization T :documentation "Remove a counter completely.")
  (if (gethash name (counter module))
      (progn
        (remhash name (counter module))
        (respond event "Removed counter ~s." name))
      (respond event "No counter called ~s found." name)))
