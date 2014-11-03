#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.twitter
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.twitter)

(define-module twitter ()
    ((streams :initform () :accessor streams))
  (:documentation "Provides access to the twitter API."))

(defmethod load-storage :after ((twitter twitter))
  (with-module-storage (twitter)
    (setf chirp:*oauth-api-key* (uc:config-tree :api-key)
          chirp:*oauth-api-secret* (uc:config-tree :api-secret) 
          chirp:*oauth-access-token* (uc:config-tree :access-token)
          chirp:*oauth-access-secret* (uc:config-tree :access-secret))))

(defmethod save-storage :before ((twitter twitter))
  (with-module-storage (twitter)
    (setf (uc:config-tree :api-key) chirp:*oauth-api-key*
          (uc:config-tree :api-secret) chirp:*oauth-api-secret*
          (uc:config-tree :access-token) chirp:*oauth-access-token*
          (uc:config-tree :access-secret) chirp:*oauth-access-secret*)))

(define-group twitter :documentation "Interact with the linked twitter account.")

(define-command (twitter verify) () (:documentation "Perform an ACCOUNT/VERIFY-CREDENTIALS call to see if the account was linked successfully.")
  (let ((user (chirp:account/verify-credentials)))
    (respond event "Linked account: ~a" (chirp:screen-name user))))

(define-command (twitter initiate-authentication) (&optional api-key api-secret) (:authorization T :documentation "Initiate the authentication process.")
  (respond event "Please visit: ~a" (chirp:initiate-authentication :api-key (or api-key chirp:*oauth-api-key*) :api-secret (or api-secret chirp:*oauth-api-secret*))))

(define-command (twitter complete-authentication) (pin) (:authorization T :documentation "Complete the authentication process.")
  (chirp:complete-authentication pin)
  (let ((user (chirp:account/verify-credentials)))
    (respond event "Successfully authenticated as ~a" (chirp:screen-name user))))

(define-command (twitter tweet) (&rest text) (:authorization T :documentation "Tweet on behalf of the linked user.")
  (chirp:tweet (format NIL "~{~a~^ ~}" text)))

(define-command (twitter stream-home) () (:authorization T :documentation "Stream the home timeline to the current channel.")
  (push (list (with-module-thread (:twitter)
                (loop
                  (chirp:stream/user #'(lambda (o) (stream-handler o event)))))
              (colleen:name (server event))
              (channel event))
        (streams module)))

(defun stream-handler (o event)
  (flet ((process-text (o len)
           (cl-ppcre:regex-replace-all
            "\\n" (chirp:xml-decode (chirp:text-with-expanded-urls o))
            (format NIL "~%~v< ~>" len))))
    (when (and o (typep o 'chirp:status))
      (let ((preamble (format NIL "[Twitter] ~a: " (chirp:screen-name (chirp:user o)))))
        (when (chirp:retweeted-status o)
          (let ((rt (chirp:statuses/show (chirp:id (chirp:retweeted-status o)))))
            (setf preamble (format NIL "~aRT ~a: " preamble (chirp:screen-name (chirp:user rt))))
            (setf o rt)))
        (respond event "~a~a"
                 preamble
                 (process-text o (length preamble))))))
  (bordeaux-threads:thread-yield)
  (not (typep o 'chirp:stream-disconnect)))

(define-command (twitter list-streams) () (:authorization T :documentation "List all working streams.")
  (respond event "~a" (streams module)))

(define-command (twitter stop-stream) (id) (:authorization T :documentation "Stop the given stream.")
  (let ((thread (gethash id (threads module))))
    (if thread
        (progn
          (bordeaux-threads:interrupt-thread thread #'(lambda () (error "STOP STREAM!")))
          (when (bordeaux-threads:thread-alive-p thread)
            (bordeaux-threads:destroy-thread thread))
          (if (bordeaux-threads:thread-alive-p thread)
              (respond event "FIXME: Destroying the stream thread failed. Thread still alive.")
              (progn
                (setf (streams module) (delete id (streams module) :key #'first :test #'string-equal))
                (respond event "Stream stopped."))))
        (respond event "No such stream."))))

(define-command (twitter follow) (screen-name) (:authorization T :documentation "Follow the specified user.")
  (chirp:friendships/create :screen-name screen-name)
  (respond event "~a followed." screen-name))

(define-command (twitter unfollow) (screen-name) (:authorization T :documentation "Unfollow the specified user.")
  (chirp:friendships/destroy :screen-name screen-name)
  (respond event "~a unfollowed." screen-name))
