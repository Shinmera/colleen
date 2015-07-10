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
   "\\{([0-9]+)(-([0-9]+)?)?(\\|(.*?))?\\}"
   template
   (lambda (template start end mstart mend regs rege)
     (declare (ignore start end mstart mend))
     (let* ((start
              (parse-integer template :start (aref regs 0) :end (aref rege 0)))
            (end
              (cond ((aref regs 2)
                     (min
                      (length args)
                      (parse-integer template :start (aref regs 2) :end (aref rege 2))))
                    ((aref regs 1)
                     (length args))
                    (T
                     (1+ start))))
            (alternative
              (when (aref regs 3)
                (subseq template (aref regs 4) (aref rege 4)))))
       (or
        (when (< start (length args))
          (format NIL "~{~a~^ ~}" (subseq args start end)))
        alternative
        "")))))

(define-command topic (&rest topicargs) (:documentation "Set the topic using the channel's template if available.")
  (let ((template (or (uc:config-tree (channel event) :template)
                      "{0-}")))
    (irc:topic (channel event) :topic (template template topicargs))))

(define-command topic-template (&rest template) (:documentation "Set a template for the current topic.")
  (setf (uc:config-tree (channel event) :template)
        (format NIL "~{~a~^ ~}" template))
  (respond event "Template set."))
