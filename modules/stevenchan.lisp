#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.stevenchan
  (:use :cl :colleen :events :alexandria :lquery))
(in-package :org.tymoonnext.colleen.mod.stevenchan)

(define-module stevenchan ()
  ((%rss-url :initarg :rss-url :initform "http://api.tymoon.eu/chan/rss" :accessor rss-url)
   (%last-id :initarg :last-id :initform 0 :accessor last-id)
   (%stream-api :initarg :stream-api :initform "http://xshinmerax.api.channel.livestream.com/2.0/livestatus.xml" :accessor stream-api)
   (%stream-live :initform NIL :accessor stream-live)))

(defmethod start ((stevenchan stevenchan))
  (with-module-thread (stevenchan)
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

       (v:debug :stevenchan "Checking livestream...")
       (handler-case
           (let ((lquery:*lquery-master-document*)
                 (data (drakma:http-request (stream-api module))))
             ($ (initialize (cl-ppcre:regex-replace-all "ls:" data "")))
             (let ((status ($ "isLive" (text) (node))))
               (unless (string-equal status (stream-live module))
                 (setf (stream-live module) status)
                 (when (string-equal status "true")
                   (v:info :Stevenchan "Stream is now live!")
                   (irc:privmsg "#Stevenchan" "[livestream] Stream is now live!" :server (get-server :tynet))))))
         (error (err)
           (v:warn :Stevenchan "Error in check-loop: ~a" err)))
       (sleep 30)
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

(define-command (stevenchan latest) () (:documentation "Get the latest post." :modulevar stevenchan)
  (apply #'respond event "#~a by ~a: ~a" (most-recent stevenchan)))

(define-command (stevenchan movie) (&optional user) (:documentation "Get information about the movie night.")
  (unless user (setf user (nick event)))
  ($ (initialize (drakma:http-request "http://movies.tymoon.eu" :external-format-in :utf-8) :type :HTML))
  (respond event "~a: ~a. Movie nights always on Saturdays, 22:00 CE(S)T. http://livestream.com/shinmera"
           user ($ "#content .box .largeCenter" (text) (node))))
