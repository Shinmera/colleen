#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.twitter
  (:use :cl :colleen :events :chirp-objects)
  (:shadowing-import-from :events :version :parameters :users :reason :sender :mode :user :parameter :address :language :resource :query :code :target)
  (:shadowing-import-from :colleen :message)
  (:shadowing-import-from :chirp-objects :start :name))
(in-package :org.tymoonnext.colleen.mod.twitter)

(defvar *config-file* (merge-pathnames "twitter.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module twitter ()
    ((streams :initform () :accessor streams))
  (:documentation "Provides access to the twitter API."))

(defmethod start ((twitter twitter))
  (with-open-file (stream *config-file* :if-does-not-exist NIL)
    (when stream
      (let ((config (yason:parse stream)))
        (setf chirp:*oauth-api-key* (gethash "api-key" config)
              chirp:*oauth-api-secret* (gethash "api-secret" config) 
              chirp:*oauth-access-token* (gethash "access-token" config)
              chirp:*oauth-access-secret* (gethash "access-secret" config)))
      (v:info :twitter "Loaded keys from config."))))

(defmethod stop ((twitter twitter))
  (with-open-file (stream *config-file* :direction :output :if-does-not-exist :create :if-exists :supersede)
    (let ((config (make-hash-table :test 'equal)))
      (setf (gethash "api-key" config) chirp:*oauth-api-key*)
      (setf (gethash "api-secret" config) chirp:*oauth-api-secret*)
      (setf (gethash "access-token" config) chirp:*oauth-access-token*)
      (setf (gethash "access-secret" config) chirp:*oauth-access-secret*)
      (yason:encode config stream))
    (v:info :twitter "Saved keys to config.")))

(define-group twitter :documentation "Interact with the linked twitter account.")

(define-command (twitter verify) () (:documentation "Perform an ACCOUNT/VERIFY-CREDENTIALS call to see if the account was linked successfully.")
  (let ((user (chirp:account/verify-credentials)))
    (respond event "Linked account: ~a" (screen-name user))))

(define-command (twitter initiate-authentication) (&optional api-key api-secret) (:authorization T :documentation "Initiate the authentication process.")
  (respond event "Please visit: ~a" (chirp:initiate-authentication :api-key (or api-key chirp:*oauth-api-key*) :api-secret (or api-secret chirp:*oauth-api-secret*))))

(define-command (twitter complete-authentication) (pin) (:authorization T :documentation "Complete the authentication process.")
  (chirp:complete-authentication pin)
  (let ((user (chirp:account/verify-credentials)))
    (respond event "Successfully authenticated as ~a" (screen-name user))))

(define-command (twitter tweet) (&rest text) (:authorization T :documentation "Tweet on behalf of the linked user.")
  (chirp:tweet (format NIL "~{~a~^ ~}" text)))

(define-command (twitter stream-home) () (:authorization T :documentation "Stream the home timeline to the current channel.")
  (push (list (with-module-thread (get-module :twitter)
                (chirp:stream/user #'(lambda (o)
                                       (when (and o (typep o 'chirp:status))
                                         (respond event "[Twitter] ~a: ~a"
                                                  (chirp:screen-name (chirp:user o))
                                                  (chirp:xml-decode (chirp:text-with-expanded-urls o))))
                                       T)))
              (colleen:name (server event))
              (channel event))
        (streams module)))

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
