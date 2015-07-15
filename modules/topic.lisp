#|
This file is a part of Colleen
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.topic
  (:nicknames #:co-topic)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.topic)

(define-module topic () ()
  (:documentation "A way to simplify topic setting to a template."))

(defun template (template args)
  (cl-ppcre:regex-replace-all
   "\\{([^{}]+)\\}"
   template
   (lambda (template start end mstart mend regs rege)
     (declare (ignore start end mstart mend))
     (or
      (gethash (subseq template (aref regs 0) (aref rege 0)) args)
      ""))))

(defun parse-args (topicargs)
  (let ((map (make-hash-table :test 'equalp)))
    (setf (gethash "message" map) (reverse topicargs))
    (loop with key = NIL
          for arg in topicargs
          do (cond ((and (< 1 (length arg)) (char= (char arg 0) #\:))
                    (setf key (subseq arg 1)))
                   (key
                    (push arg (gethash key map)))))
    (maphash (lambda (key val)
               (setf (gethash key map) (format NIL "~{~a~^ ~}" (reverse val))))
             map)
    map))

(defun merge-table-into (from to)
  (loop for k being the hash-keys of from
        for v being the hash-values of from
        do (setf (gethash k to) v))
  to)

(define-command topic (&rest topicargs) (:documentation "Set the topic using the channel's template if available. Use prefix names with a colon to indicate a placeholder.")
  (let ((template (or (uc:config-tree (channel event) :template)
                      "{message}"))
        (arguments (or (uc:config-tree (channel event) :arguments)
                       (make-hash-table :test 'equalp))))
    (merge-table-into (parse-args topicargs) arguments)
    (irc:topic (channel event) :topic (template template arguments))))

(define-command topic-template (&rest template) (:documentation "Set a template for the current topic. Use {name} to enter placeholders.")
  (setf (uc:config-tree (channel event) :template)
        (format NIL "~{~a~^ ~}" template)
        (uc:config-tree (channel event) :arguments)
        (make-hash-table :test 'equalp))
  (respond event "Template set."))
