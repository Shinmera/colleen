#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.stevenchan
  (:use :cl :colleen :alexandria :lquery)
  (:shadowing-import-from :colleen :restart))
(in-package :org.tymoonnext.colleen.mod.stevenchan)

(define-module stevenchan ()
  ((%rss-url :initarg :rss-url :initform "http://api.tymoon.eu/chan/rss" :accessor rss-url :allocation :class)
   (%last-id :initarg :last-id :initform 0 :accessor last-id :allocation :class)
   (%thread :allocation :class :accessor thread)))

(defmethod start ((stevenchan stevenchan))
  (setf (thread stevenchan)
        (bordeaux-threads:make-thread #'(lambda () (check-loop stevenchan)))))

(defmethod stop ((stevenchan stevenchan)))

(defun check-loop (module)
  (loop while (active module)
     do (destructuring-bind (id author link) (most-recent module)
          (when (> id (last-id module))
            (v:info :Stevenchan "New post: ~a by ~a: ~a" id author link)
            (setf (last-id module) id)
            (irc:privmsg "#Stevenchan" (format NIL "New post: ~a by ~a: ~a" id author link) :server (get-server :tynet))))
       (sleep 10)))

(defgeneric most-recent (stevenchan &optional rss-url))
(defmethod most-recent ((stevenchan stevenchan) &optional (rss-url (rss-url stevenchan)))
  (let* ((node ($ (initialize (drakma:http-request rss-url) :type :XML) "item" (first)))
         (id (parse-integer (subseq (string-trim " " ($ node "title" (text) (node))) 1)))
         (author (string-trim " " ($ node "author" (text) (node))))
         (link (string-trim " " ($ node "link" (text) (node)))))
    (list id author link)))

(define-group stevenchan stevenchan :documentation "Interact with Stevenchan.")

(define-command (stevenchan latest) stevenchan (&optional board) (:documentation "Get the latest post.")
  (apply #'respond event "#~a by ~a: ~a" (most-recent stevenchan)))
