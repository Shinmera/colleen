#|
  This file is a part of Colleen
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.stevenchan
  (:use :cl :colleen :events :alexandria :lquery)
  (:nicknames #:co-stevenchan))
(in-package :org.tymoonnext.colleen.mod.stevenchan)

(define-module stevenchan ()
  ((%rss-url :initarg :rss-url :initform "http://chan.tymoon.eu/api/purplish/atom" :accessor rss-url)
   (%last-id :initarg :last-id :initform 0 :accessor last-id)
   (%stream-api :initarg :stream-api :initform "http://xshinmerax.api.channel.livestream.com/2.0/livestatus.xml" :accessor stream-api)
   (%stream-live :initform NIL :accessor stream-live)))

(defmethod start ((stevenchan stevenchan))
  (schedule-timer 'check-stuff :every 60))

(define-timer check-stuff () (:type :single :documentation "Checks for new posts and livestream status.")
  (let ((*current-server* (server :tynet)))
    (v:debug :stevenchan "Checking for new posts...") 
    (handler-case 
        (destructuring-bind (id author link) (most-recent module)
          (when (> id (last-id module))
            (v:debug :Stevenchan "New post: ~a by ~a: ~a" id author link)
            (setf (last-id module) id)
            (irc:privmsg "#Stevenchan" (format NIL "New post: ~a by ~a: ~a" id author link))))
      (error (err)
        (v:warn :Stevenchan "Error in checker ~a" err)))

    (v:debug :stevenchan "Checking livestream...")
    (handler-case
        (let ((lquery:*lquery-master-document*)
              (data (drakma:http-request (stream-api module))))
          ($ (initialize (cl-ppcre:regex-replace-all "ls:" data "")))
          (let ((status ($ "isLive" (text) (node))))
            (unless (string-equal status (stream-live module))
              (setf (stream-live module) status)
              (when (string-equal status "true")
                (v:debug :Stevenchan "Stream is now live!")
                (irc:privmsg "#Stevenchan" "[livestream] Stream is now live!")
                (irc:privmsg "#Stevenchan" (format NIL "~{~a: ~} ^" (remove (nick (server :tynet)) (users "#Stevenchan") :test #'string-equal)))))))
      (error (err)
        (v:warn :Stevenchan "Error in checker ~a" err)))))

(defgeneric most-recent (stevenchan &optional rss-url))
(defmethod most-recent ((stevenchan stevenchan) &optional (rss-url (rss-url stevenchan)))
  (let* ((lquery:*lquery-master-document*)
         (plump:*tag-dispatchers* ())
         (drakma:*text-content-types* '(("application" . "atom+xml")))
         (node ($ (initialize (drakma:http-request rss-url)) "entry" (first)))
         (id ($ node "id" (text) (node)))
         (id (parse-integer (subseq id (+ (search "#post-" id) (length "#post-")))))
         (author (string-trim " " ($ node "author name" (text) (node))))
         (link (string-trim " " ($ node "link" (attr :href) (node)))))
    (list id author link)))

(define-group stevenchan :documentation "Interact with Stevenchan.")

(define-command (stevenchan latest) () (:documentation "Get the latest post." :modulevar stevenchan)
  (apply #'respond event "#~a by ~a: ~a" (most-recent stevenchan)))

(define-command (stevenchan movie) (&optional user) (:documentation "Get information about the movie night.")
  (unless user (setf user (nick event)))
  (let ((lquery:*lquery-master-document*))
    ($ (initialize (drakma:http-request "http://movies.tymoon.eu" :external-format-in :utf-8)))
    (respond event "~a: ~a. Movie nights always on Saturdays, 22:00 CE(S)T. http://livestream.com/shinmera"
             user ($ "#content .box .largeCenter" (text) (node)))))
