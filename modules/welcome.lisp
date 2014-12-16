#|
 This file is a part of Colleen
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.welcome
  (:nicknames #:co-welcome)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.welcome)

(define-module welcome ()
    ((%might-greet :initform () :accessor might-greet))
  (:documentation "Provides functions to greet and welcome users."))

(defmethod start ((welcome welcome))
  (with-module-storage (welcome)
    (unless (uc:config-tree :personal)
      (setf (uc:config-tree :personal)
            (make-hash-table :test 'equalp)))
    (unless (uc:config-tree :channel)
      (setf (uc:config-tree :channel)
            (make-hash-table :test 'equalp)))))

(defun real-place (event)
  (format NIL "~a/~a" (name (server event)) (channel event)))

(defun real-nick (event)
  (format NIL "~a/~a" (name (server event)) (nick event)))

(defun definitive-nick (event)
  (format NIL "~a/~a/~a" (name (server event)) (channel event) (nick event)))

(defun is-active-in (event)
  (find (real-place event) (uc:config-tree :active-in) :test #'string-equal))

(defun random-elt (array)
  (aref array (random (length array))))

(defparameter *default-responses* #("Hi." "Hey." "Hello." "Hullo." "Heyo." "Heya." ; Standard greetings
                                    "Hi." "Hey." "Hello." "Hullo." "Heyo." "Heya." ; Dupe for double probability
                                    "Shut up." "Go away." "Get lost." "No." ; Mean greetings
                                    "*grunt*" "*snirk*" "*snicker*" ; Noises
                                    "<GREETING-OF-CHOICE>" "Good <APPROPRIATE-PART-OF-DAY>." ;; Robotic
                                    ))
(defparameter *default-greetings* '("hello" "hullo" "hi" "mornin" "morning" "evening" "afternoon" "night" "heya" "heyo"
                                    "howdy" "eyy" "sup" "butts" "scream" "screech" "u+g+h+"))

(defun is-greeting (string)
  (let ((string (string-downcase (string-trim " '.~-!?" string))))
    (loop for greeting in *default-greetings*
          thereis (cl-ppcre:scan (format NIL "\\b~a\\b" greeting) string))))

(define-handler (join-event event) ()
  (when (is-active-in event)
    (pushnew (definitive-nick event) (might-greet module) :test #'string-equal)))

(define-handler (privmsg-event event) ()
  (let ((definitive-nick (definitive-nick event)))
    (when (and (is-active-in event)
               (find definitive-nick (might-greet module) :test #'string-equal))
      (when (is-greeting (message event))
        (respond event "~a" (random-elt (or (uc:config-tree :personal (real-nick event))
                                            (uc:config-tree :channel (real-place event))
                                            *default-responses*))))
      (setf (might-greet module)
            (delete definitive-nick (might-greet module) :test #'string-equal)))))

(define-group welcome :documentation "Manage welcoming options.")

(define-command (welcome activate) () (:documentation "Activate welcoming for this channel." :authorization T)
  (pushnew (real-place event) (uc:config-tree :active-in) :test #'string-equal)
  (respond event "Welcoming activated on this channel."))

(define-command (welcome deactivate) () (:documentation "Deactivate welcoming for this channel." :authorization T)
  (setf (uc:config-tree :active-in)
        (delete (real-place event) (uc:config-tree :active-in) :test #'string-equal))
  (respond event "Welcoming deactivated on this channel."))

(define-command (welcome set) (&rest message) (:documentation "Set a personal greeting. You can set multiple by separating with |. If you want to disable personal greetings, don't provide a message.")
  (let ((greetings (cl-ppcre:split "\\|" (format NIL "~{~a~^ ~}" message))))
    (if greetings
        (progn (setf (uc:config-tree :personal (real-nick event)) (coerce greetings 'vector))
               (respond event "Personal greetings set."))
        (progn (setf (uc:config-tree :personal (real-nick event)) NIL)
               (respond event "Personal greetings removed.")))))

(define-command (welcome add) (&rest message) (:documentation "Add a personal greeting." :authorization T)
  (let ((greetings (coerce (uc:config-tree :personal (real-nick event)) 'list)))
    (pushnew (format NIL "~{~a~^ ~}" message) greetings :test #'string-equal)
    (setf (uc:config-tree :personal (real-nick event)) (coerce greetings 'vector))
    (respond event "Personal greeting added.")))

(define-command (welcome clear) () (:documentation "Clear all personal welcome messages.")
  (setf (uc:config-tree :personal (real-nick event)) NIL)
  (respond event "Welcome messages for ~a cleared." (channel event)))

(define-command (welcome force) (network user &rest message) (:documentation "Force a greeting on a user. See WELCOME SET for more info." :authorization T)
  (let ((greetings (cl-ppcre:split "\\|" (format NIL "~{~a~^ ~}" message))))
    (if greetings
        (progn (setf (uc:config-tree :personal (format NIL "~a/~a" network user)) (coerce greetings 'vector))
               (respond event "Personal greetings for ~a set." user))
        (progn (setf (uc:config-tree :personal (format NIL "~a/~a" network user)) NIL)
               (respond event "Personal greetings for ~a removed." user)))))

(define-command (welcome channel set) (&rest message) (:documentation "Set a channel greeting. You can set multiple by separating with |." :authorization T)
  (let ((greetings (cl-ppcre:split "\\|" (format NIL "~{~a~^ ~}" message))))
    (if greetings
        (progn (setf (uc:config-tree :channel (real-place event)) (coerce greetings 'vector))
               (respond event "Channel greetings set."))
        (progn (setf (uc:config-tree :channel (real-place event)) NIL)
               (respond event "Channel greetings removed.")))))

(define-command (welcome channel add) (&rest message) (:documentation "Add a greeting for this channel." :authorization T)
  (let ((greetings (coerce (uc:config-tree :channel (real-place event)) 'list)))
    (pushnew (format NIL "~{~a~^ ~}" message) greetings :test #'string-equal)
    (setf (uc:config-tree :channel (real-place event)) (coerce greetings 'vector))
    (respond event "Greeting for ~a added." (channel event))))

(define-command (welcome channel clear) () (:documentation "Clear all channel-specific greetings." :authorization T)
  (setf (uc:config-tree :channel (real-place event)) NIL)
  (respond event "Greetings for ~a cleared." (channel event)))
