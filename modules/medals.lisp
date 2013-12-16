#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.medals
  (:use :cl :colleen :events)
  (:shadowing-import-from :colleen :restart))
(in-package :org.tymoonnext.colleen.mod.medals)

(defvar *save-file* (merge-pathnames "medals-save.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module medals ()
  ((medals :initform (make-hash-table :test 'equal) :accessor medals))
  (:documentation "Award medals to users."))

(defmethod start ((medals medals))
  (%load medals))

(defmethod stop ((medals medals))
  (%save medals))

(defmethod %load ((module medals))
  (with-open-file (stream *save-file* :if-does-not-exist NIL)
    (when stream
      (let ((medals (yason:parse stream)))
        (loop for k being the hash-keys of medals
           for v being the hash-values of medals
           do (setf (gethash k (medals module)) v))))))

(defmethod %save ((module medals))
  (with-open-file (stream *save-file* :direction :output :if-does-not-exist :create :if-exists :supersede)
    (yason:encode (medals module) stream)))

(defun %award (module event user medal)
  (setf medal (format NIL "~{~a~^ ~}" medal))
  (pushnew medal (gethash (string-downcase user) (medals module)) :test #'string-equal)
  (respond event "~a has been awarded the ~a medal." user medal))

(define-group medals :documentation "Manage medals.")

(define-command (medals save) () (:authorization T :documentation "Perform a save to disk.")
  (%save module))

(define-command (medals load) () (:authorization T :documentation "Perform a load from disk.")
  (%load module))

(define-command (medals show) (&optional user) (:documentation "List the medals a user has.")
  (unless user (setf user (nick event)))
  (respond event "~a has the following medals: ~{~a~^, ~}" user (gethash (string-downcase user) (medals module))))

(define-command (medals award) (user &rest medal) (:authorization T :documentation "Award a medal to a user.")
  (%award module event user medal))

(define-command (medals revoke) (user &rest medal) (:authorization T :documentation "Revoke a medal from a user.")
  (setf medal (format NIL "~{~a~^ ~}" medal)
        user (string-downcase user))
  (setf (gethash user (medals module)) (delete medal (gethash user (medals module)) :test #'string-equal))
  (respond event "The ~a medal has been revoked from ~a" medal user))

(define-command award (user &rest medal) (:authorization T :documentation "Award a medal to a user.")
  (%award module event user medal))
