#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.urlinfo
  (:use :cl :colleen :events)
  (:nicknames :co-urlinfo))
(in-package :org.tymoonnext.colleen.mod.urlinfo)

(define-module urlinfo ()
    ((streams :initform () :accessor streams))
  (:documentation "Follows URLs and reads information about the target site."))

(defparameter *url-regex* (cl-ppcre:create-scanner "((http|https)://[\\w\\d\\-.]+\\.\\w{2,}([\\w\\d\\-%+?=&@#.;/]*)?)"))
(defparameter *title-regex* (cl-ppcre:create-scanner "<title>((\\s|.)*?)</title>" :case-insensitive-mode T))
(defparameter *html-types* '("text/html" "application/xhtml+xml"))

(defun urlinfo (url)
  (multiple-value-bind (content status headers uri) (drakma:http-request url)
    (when (= status 200)
      (let ((target-url (make-string-output-stream)))
        (puri:render-uri uri target-url)
        (setf target-url (get-output-stream-string target-url))
        (if (find (cdr (assoc :content-type headers)) *html-types*
                  :test #'(lambda (a b) (search b a)))
            (let ((title (nth-value 1 (cl-ppcre:scan-to-strings *title-regex* content))))
              (if title
                  (format NIL "Title: “~a”~:[ at ~a~;~*~]"
                          (plump:decode-entities (aref title 0))
                          (string-equal target-url url) target-url)
                  (format NIL "Invalid HTML document~:[ at ~a~;~*~]"
                          (string-equal target-url url) target-url)))
            (format NIL "~a~:[ at ~a~;~*~]"
                    (cdr (assoc :content-type headers))
                    (string-equal target-url url) target-url))))))

(defun command-p (message)
  (loop for prefix in (bot-config :command :prefix)
        when (and (> (length message) (length prefix))
                  (string= message prefix :end1 (length prefix)))
          do (return T)
        finally (return NIL)))

(define-handler (privmsg-event event) ()
  (when (and (find (format NIL "~a/~a" (name (server event)) (channel event))
                   (uc:config-tree :active-in) :test #'string-equal)
             (not (command-p (message event))))
    (cl-ppcre:do-register-groups (url) (*url-regex* (message event))
      (respond event "[urlinfo] ~a" (urlinfo url)))))

(define-group url :documentation "Get information about an URL.")

(define-command (url about) (url) (:documentation "Get information about an url.")
  (respond event (urlinfo url)))

(define-command (url activate) (&optional channel server) (:documentation "Activate automatic url info.")
  (pushnew (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))
           (uc:config-tree :active-in) :test #'string-equal)
  (respond event "Automatic URL info activated for ~a/~a"
           (or server (name (server event))) (or channel (channel event))))

(define-command (url deactivate) (&optional channel server) (:documentation "Activate automatic url info.")
  (setf (uc:config-tree :active-in)
        (delete (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))
                (uc:config-tree :active-in) :test #'string-equal))
  (respond event "Automatic URL info deactivated for ~a/~a"
           (or server (name (server event))) (or channel (channel event))))
