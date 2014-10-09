#|
  This file is a part of Colleen
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.medals
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.medals)

(defvar *save-file* (merge-pathnames "medals-save.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module medals () ()
  (:documentation "Award medals to users."))

(defun %award (module event user medal)
  (with-module-storage (module)
    (setf medal (format NIL "~{~a~^ ~}" medal))
    (pushnew medal (uc:config-tree (string-downcase user)) :test #'string-equal)
    (respond event "~a has been awarded the ~a medal." user medal)))

(define-group medals :documentation "Manage medals.")

(define-command (medals show) (&optional user) (:documentation "List the medals a user has.")
  (unless user (setf user (nick event)))
  (respond event "~a has the following medals: ~{~a~^, ~}" user (uc:config-tree (string-downcase user))))

(define-command (medals award) (user &rest medal) (:authorization T :documentation "Award a medal to a user.")
  (%award module event user medal))

(define-command (medals revoke) (user &rest medal) (:authorization T :documentation "Revoke a medal from a user.")
  (setf medal (format NIL "~{~a~^ ~}" medal)
        user (string-downcase user))
  (setf (uc:config-tree user)
        (delete medal (uc:config-tree user) :test #'string-equal))
  (respond event "The ~a medal has been revoked from ~a" medal user))

(define-command award (user &rest medal) (:authorization T :documentation "Award a medal to a user.")
  (%award module event user medal))
