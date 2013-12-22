#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.stevenchan
  (:use :cl :colleen :events :alexandria :lquery)
  (:shadowing-import-from :colleen :restart))
(in-package :org.tymoonnext.colleen.mod.stevenchan)

(define-module stevenchan ()
  ((%rss-url :initarg :rss-url :initform "http://api.tymoon.eu/chan/rss" :accessor rss-url :allocation :class)
   (%last-id :initarg :last-id :initform 0 :accessor last-id :allocation :class)))

(defmethod start ((stevenchan stevenchan))
  (with-module-thread stevenchan
    (check-loop stevenchan)))

(defun check-loop (module)
  (v:trace :stevenchan "Entering check-loop!")
  (loop
     do (v:debug :stevenchan "Checking for new posts...") 
       (handler-case 
            (destructuring-bind (id author link) (most-recent module)
              (when (> id (last-id module))
                (v:info :Stevenchan "New post: ~a by ~a: ~a" id author link)
                (setf (last-id module) id)
                (irc:privmsg "#Stevenchan" (format NIL "New post: ~a by ~a: ~a" id author link) :server (get-server :tynet))))
          (error (err)
            (v:warn :Stevenchan "Error in check-loop: ~a" err)))
       (sleep 10)
     while (active module))
  (v:trace :stevenchan "Leaving check-loop!"))

(defgeneric most-recent (stevenchan &optional rss-url))
(defmethod most-recent ((stevenchan stevenchan) &optional (rss-url (rss-url stevenchan)))
  (let* ((node ($ (initialize (drakma:http-request rss-url) :type :XML) "item" (first)))
         (title (string-trim " " ($ node "title" (text) (node))))
         (id (parse-integer (subseq title (1+ (search "#" title)))))
         (author (string-trim " " ($ node "author" (text) (node))))
         (link (string-trim " " ($ node "link" (text) (node)))))
    (list id author link)))

(define-group stevenchan :documentation "Interact with Stevenchan.")

(define-command (stevenchan latest) (&optional board) (:documentation "Get the latest post." :modulevar stevenchan)
  (apply #'respond event "#~a by ~a: ~a" (most-recent stevenchan)))

(define-command (stevenchan movie) (&optional user) (:documentation "Get information about the movie night.")
  (unless user (setf user (nick event)))
  ($ (initialize (drakma:http-request "http://movies.tymoon.eu" :external-format-in :utf-8) :type :HTML))
  (respond event "~a: ~a. Movie nights always on Saturdays, 22:00 CE(S)T. http://justin.tv/stc_mv (pw: faggotry)"
           user ($ "#content .box .largeCenter" (text) (node))))
