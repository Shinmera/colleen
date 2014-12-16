#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.profile
  (:nicknames #:co-profile)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.profile)

(defvar *nickserv-status-regex* (cl-ppcre:create-scanner "STATUS (.+) ([0-3])"))
(defvar *pending-nickserv-ops* (make-hash-table :test 'equalp))
(defvar *nick-assoc-map* (make-hash-table :test 'equalp))

(defvar *time-format* '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))
(defun format-time (universal-time)
  (local-time:format-timestring NIL (local-time:universal-to-timestamp universal-time) :format *time-format*))

(defun real-nick (event)
  (format NIL "~a/~a" (name (server event)) (nick event)))

(defun fire-nickserv-auth-test (real-nick nick success-fun fail-fun)
  (setf (gethash real-nick *pending-nickserv-ops*)
        (list success-fun fail-fun))
  (irc:privmsg "NickServ" (format nil "STATUS ~a" nick)))

(defmacro with-nickserv-authenticated ((event-form) &body body)
  (let ((event (gensym "EVENT")))
    `(let ((,event ,event-form))
       (fire-nickserv-auth-test
        (real-nick ,event) (nick ,event)
        #'(lambda () ,@body)
        #'(lambda () (respond ,event "~a: NickServ auth failed!" (nick ,event)))))))

(defmacro with-profile ((profile-var name-form &optional (event-form 'event)) &body body)
  (let ((event (gensym "EVENT")))
    `(let ((,profile-var (uc:config-tree ,name-form))
           (,event ,event-form))
       (if ,profile-var
           (progn ,@body)
           (respond ,event "~a: No such profile found!" (nick ,event))))))

(defmacro with-password ((profile-form password-form &optional (event-form 'event)) &body body)
  (let ((event (gensym "EVENT")))
    `(if (string= (password ,profile-form) ,password-form)
         (progn ,@body)
         (let ((,event ,event-form))
           (respond ,event "~a: Invalid password!" (nick ,event))))))

(defun is-associated (event profile)
  (find (name profile) (gethash (real-nick event) *nick-assoc-map*) :test #'string-equal))

(define-module profile () ()
  (:documentation "Provides user profiles that can be used for informations, messaging and nick linking"))

(defmethod start ((profile profile))
  (unless (eq (hash-table-test (storage profile)) 'equalp)
    (setf (storage profile) (make-hash-table :test 'equalp))))

(defclass user-profile ()
  ((%name :initarg :name :initform (error "NAME required") :accessor name)
   (%nicks :initarg :nicks :initform () :accessor nicks)
   (%fields :initarg :fields :initform (make-hash-table :test 'equalp) :accessor fields)
   (%last-message :initarg :last-message :initform "" :accessor last-message)
   (%register-date :initarg :register-date :initform (get-universal-time) :accessor register-date)
   (%password :initarg :password :initform (error "PASSWORD required") :accessor password)
   (%is-granting :initarg :is-granting :initform NIL :accessor is-granting)))

(defmethod initialize-instance :after ((profile user-profile) &key)
  (with-module (module)
    (with-module-storage (module)
      (setf (uc:config-tree (name profile))
            profile))))

(defmethod print-object ((profile user-profile) stream)
  (print-unreadable-object (profile stream :type T)
    (format stream "~a" (name profile)))
  profile)

(uc:define-serializer (user-profile profile T)
  (make-array 6 :initial-contents (list (name profile) (nicks profile) (fields profile) (last-message profile) (register-date profile) (password profile))))

(uc:define-deserializer (user-profile array T)
  (make-instance 'user-profile
                 :name (aref array 0)
                 :nicks (aref array 1)
                 :fields (aref array 2)
                 :last-message (aref array 3)
                 :register-date (aref array 4)
                 :password (aref array 5)))

(define-group profile :documentation "Manage user profiles")

;;; Setting

(define-command (profile register) (name password) (:documentation "Register a new profile linked to your current nick called NAME, secured with PASSWORD.")
  (let ((real-nick (real-nick event)))
    (if (uc:config-tree name)
        (respond event "~a: A profile with that name already exists!" (nick event))
        (progn
          (make-instance 'user-profile :name name :password password :nicks (list (real-nick event)))
          (pushnew name (gethash real-nick *nick-assoc-map*) :test #'string-equal)
          (respond event "~a: Profile created!" (nick event))))))

(define-command (profile delete) (name password) (:documentation "Delete the profile with NAME.")
  (let ((real-nick (real-nick event)))
    (with-profile (profile name)
      (with-password (profile password)
        (remhash name (storage module))
        (setf (gethash real-nick *nick-assoc-map*)
              (delete name (gethash real-nick *nick-assoc-map*) :test #'string-equal))
        (respond event "~a: Profile deleted!" (nick event))))))

(define-command (profile link) (name password) (:documentation "Link the nick with a profile. The profile password is required and you need to be registered with nickserv.")
  (with-profile (profile name)
    (with-password (profile password)
      (let ((real-nick (real-nick event)))
        (flet ((link ()
                 (pushnew real-nick (nicks profile) :test #'string-equal)
                 (pushnew name (gethash real-nick *nick-assoc-map*) :test #'string-equal)
                 (respond event "~a: Successfully linked to ~a." (nick event) name)))
          (if (auth-p (nick event))
              (link)
              (with-nickserv-authenticated (event) (link))))))))

(define-command (profile associate) () (:documentation "Try to login to all linked profiles.")
  (let ((real-nick (real-nick event)))
    (with-nickserv-authenticated (event)
      ;; We don't have to worry about module locking here since we know
      ;; it will be executed in a safe context.
      (loop with associated = ()
            for profile being the hash-values of (storage module)
            when (find real-nick (nicks profile) :test #'string-equal)
              do (pushnew (name profile) (gethash real-nick *nick-assoc-map*)
                          :test #'string-equal)
                 (push (name profile) associated)
                 (when (is-granting profile)
                   (add-to-auth (nick event) (format NIL "Associated with granting profile ~a" (name profile)))
                   (respond event "~a: You are now logged in as per granting of ~a." (nick event) (name profile)))
            finally (if associated
                        (respond event "~a: Associated with ~{~a~^, ~}" (nick event) associated)
                        (respond event "~a: No matching profiles found!" (nick event)))))))

(define-command (profile change-password) (name password new-password) (:documentation "Change the password of a profile.")
  (with-profile (profile name)
    (with-password (profile password)
      (setf (password profile) new-password)
      (respond event "~a: Password changed." (nick event)))))

(define-command (profile set granting) (name granting-p) (:documentation "Change the granting (authentication) setting of a profile." :authorization T)
  (with-profile (profile name)
    (setf granting-p (or (string-equal granting-p "T") (string-equal granting-p "true"))
          (is-granting profile) granting-p)
    (respond event "~a: Set granting of ~a to ~a" (nick event) name granting-p)))

(define-command (profile set) (name field &rest value) (:documentation "Set the value of FIELD on a profile called NAME to VALUE.")
  (with-profile (profile name)
    (if (is-associated event profile)
        (progn (setf (gethash field (fields profile))
                     (format NIL "~{~a~^ ~}" value))
               (respond event "~a: Value changed." (nick event)))
        (respond event "~a: You are not associated with ~a." (nick event) (name profile)))))


;;; Retrieval

(define-command (profile about) (&optional name) (:documentation "Show the profile associated with NAME.")
  (unless name (setf name (nick event)))
  (with-profile (profile name)
    (respond event "~a Registered on ~a. ~{~a~^, ~}"
             (name profile) (format-time (register-date profile))
             (loop for k being the hash-keys of (fields profile)
                   for v being the hash-values of (fields profile)
                   collect (format NIL "~a: ~s" k v)))))

(define-command (profile about links) (&optional name) (:documentation "List all linked nicks associated with NAME.")
  (unless name (setf name (nick event)))
  (with-profile (profile name)
    (respond event "~a is linked to ~{~a~^, ~}"
             (name profile) (nicks profile))))

(define-command (profile about last-message) (&optional name) (:documentation "Show the last recorded message for NAME.")
  (unless name (setf name (nick event)))
  (with-profile (profile name)
    (respond event "~a's last recorded message is: ~a"
             (name profile) (last-message profile))))

;;; Event handling

(define-handler (privmsg-event event) ()
  (let ((real-nick (real-nick event)))
    (loop for name in (gethash real-nick *nick-assoc-map*)
          do (setf (last-message (uc:config-tree name)) (message event)))))

(define-handler (join-event event) ()
  (let ((real-nick (real-nick event)))
    (loop with associated = ()
          for profile being the hash-values of (storage module)
          when (and (find real-nick (nicks profile) :test #'string-equal)
                    (find (name profile) (gethash real-nick *nick-assoc-map*) :test #'string-equal))
            do (push profile associated)
          finally (when associated
                    (v:info :profile "Attempting to automatically associate ~a with ~a on JOIN." real-nick associated)
                    (fire-nickserv-auth-test
                     real-nick (nick event)
                     #'(lambda ()
                         (dolist (profile associated)
                           (pushnew (name profile) (gethash real-nick *nick-assoc-map*) :test #'string-equal)
                           (when (is-granting profile)
                             (add-to-auth (nick event) (format NIL "Associated with granting profile ~a" (name profile))))))
                     #'(lambda ()))))))

(define-handler (notice-event event) ()
  (when (string-equal "NickServ" (nick event))
    (cl-ppcre:register-groups-bind (user level) (*nickserv-status-regex* (message event))
      (let* ((real-nick (format NIL "~a/~a" (name (server event)) user))
             (op (gethash real-nick *pending-nickserv-ops*)))
        (when op
          (if (string= level "3")
              (progn
                (remhash real-nick *pending-nickserv-ops*)
                (funcall (first op)))
              (progn
                (v:warn :profile "Received bad NickServ STATUS for user ~a: ~d" user level)
                (funcall (second op)))))))))

(defmacro define-removing-handler (event-type)
  `(define-handler (,event-type event) ()
     (remhash (nick event) *nick-assoc-map*)))

(define-removing-handler part-event)
(define-removing-handler quit-event)
(define-removing-handler kick-event)
(define-removing-handler nick-event)
