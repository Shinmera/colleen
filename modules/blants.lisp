#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.blants
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.blants)

(define-module blants () ())

(defparameter *thanks-match* (cl-ppcre:create-scanner "[Tt]hanks[,]? ([a-zA-Z]+)$"))
(defparameter *bless-match* (cl-ppcre:create-scanner "[Bb]less you[,]? ([a-zA-Z]+)$"))

(defun cut-to-first-vocal (string)
  (loop for i from 0 below (length string)
     until (find (aref string i) '(#\a #\e #\i #\o #\u))
     finally (return (subseq string i))))

(define-handler (privmsg-event event) ()
  (cl-ppcre:register-groups-bind (name) (*thanks-match* (message event))
    (sleep 2)
    (respond event "...Th~a" (cut-to-first-vocal name)))
  (cl-ppcre:register-groups-bind (name) (*bless-match* (message event))
    (sleep 2)
    (respond event "...Bl~a" (cut-to-first-vocal name))))

(define-command thanks () (:documentation "Thanks you." :eventvar event)
  (respond event "Thanks, ~a" (nick event))
  (sleep 2)
  (respond event "...Th~a" (cut-to-first-vocal (nick event))))
