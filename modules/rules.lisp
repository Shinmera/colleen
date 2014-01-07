#|
This file is a part of Colleen
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.rules
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.rules)

(define-module rules () 
    ((%rules :initform (make-hash-table :test 'equalp) :accessor rules)
     (%whitelist :initform () :accessor whitelist))
  (:documentation "Primitive rule enforcement module."))

(defparameter *types* '(:KICK :BAN :KILL :MUTE :SCOLD :REWARD))
(defparameter *fields* '(:regex :type :probation :breach-msg :apology-msg :punish-msg))
(defparameter *save-file* (merge-pathnames "rules-save.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(defclass rule ()
  ((%name :initarg :name :initform (error "Name required.") :accessor name)
   (%regex :initarg :regex :initform (error "Regex required.") :accessor regex)
   (%type :initarg :type :initform :KICK :accessor rule-type)
   (%probation :initarg :probation :initform 3 :accessor probation)
   (%breach-msg :initarg :breach-msg :initform "$NICK$: You breached the \"$RULE$\" rule. You are now on probation ($COUNT$ left)." :accessor breach-msg)
   (%apology-msg :initarg :apology-msg :initform "$NICK$: Thank you. Please refrain from breaching this rule again." :accessor apology-msg)
   (%punish-msg :initarg :punish-msg :initform "Thoughtcrime." :accessor punish-msg)
   (%users-on-probation :initarg :users-on-probabiont :initform (make-hash-table :test 'equalp) :accessor users-on-probation)
   (%active-on :initarg :active-on :initform () :accessor active-on))
  (:documentation "Class for containing rulesets."))

(defmethod print-object ((rule rule) stream)
  (print-unreadable-object (rule stream :type T)
    (format stream "~a \"~a\"" (rule-type rule) (regex rule))))

(defmethod yason:encode ((rule rule) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "regex" (regex rule))
      (yason:encode-object-element "type" (string-upcase (rule-type rule)))
      (yason:encode-object-element "probation" (probation rule))
      (yason:encode-object-element "breach-msg" (breach-msg rule))
      (yason:encode-object-element "apology-msg" (apology-msg rule))
      (yason:encode-object-element "punish-msg" (punish-msg rule))
      (yason:encode-object-element "active-on" (active-on rule)))))

(defmethod start ((rules rules))
  (with-open-file (stream *save-file* :if-does-not-exist NIL)
    (when stream
      (let ((data (yason:parse stream)))
        (loop for name being the hash-key of data
           for object being the hash-value of data
           do (setf (gethash name (rules rules))
                    (make-instance
                     'rule :name name
                     :regex (gethash "regex" object)
                     :type (find-symbol (gethash "type" object) :KEYWORD)
                     :probation (gethash "probation" object)
                     :breach-msg (gethash "breach-msg" object)
                     :apology-msg (gethash "apology-msg" object)
                     :punish-msg (gethash "punish-msg" object)
                     :active-on (gethash "active-on" object))))
        (v:info :rules "Loaded ~d rules." (hash-table-count data)))))
  (setf (whitelist rules) (config-tree :rules :whitelist)))

(defmethod stop ((rules rules))
  (with-open-file (stream *save-file* :direction :output :if-exists :supersede :if-does-not-exist :create)
    (yason:encode (rules rules) stream)
    (v:info :rules "Saved ~d rules." (hash-table-count (rules rules))))
  (setf (config-tree :rules :whitelist) (whitelist rules)))

(defun rule-format (event rule probation-count message)
  (format-message event message
                  (cons "\\$RULE\\$" (name rule))
                  (cons "\\$COUNT\\$" (format NIL "~a" probation-count))))


(defparameter *apology-regex* (cl-ppcre:create-scanner "^(Colleen:\\s*)?(Sorry for|I apologize for|I'm sorry for)\\s+(breaking|violating|breaching|not minding|disregarding|saying)?\\s*(the\\s+)?(\\w+).*" :case-insensitive-mode T))
(define-handler (privmsg-event event) ()
  (when (not (find (format NIL "~a/~a" (string-upcase (name (server event))) (string-upcase (nick event)))
                   (whitelist module) :test #'string-equal))
    (let ((name-format (format NIL "~a/~a" (name (server event)) (nick event)))
          (server-format (format NIL "~a/~a" (name (server event)) (channel event))))
      (unless (cl-ppcre:register-groups-bind (NIL NIL NIL NIL name) (*apology-regex* (message event))
                (let ((rule (gethash name (rules module))))
                  (when (and rule (find server-format (active-on rule) :test #'string-equal))
                    (if (gethash name-format (users-on-probation rule))
                        (progn
                          (v:info :rules "~a is apologizing for ~a" (nick event) name)
                          (remhash name-format (users-on-probation rule))
                          (respond event (rule-format event rule (probation rule) (apology-msg rule))))
                        (respond event "~a: You don't need to apologize for ~a, silly!" (nick event) (name rule))))
                  T))
        
        (loop for rule being the hash-value of (rules module)
           if (and (find server-format (active-on rule) :test #'string-equal)
                   (cl-ppcre:scan (regex rule) (string-downcase (message event))))
           do 
             (let ((probation-count (gethash name-format (users-on-probation rule))))
               (unless probation-count
                 (setf probation-count (probation rule)))
               (setf probation-count (1- probation-count))

               (if (> 0 probation-count)
                   (let ((message (rule-format event rule probation-count (punish-msg rule))))
                     (v:warn :rules "~a violated rule ~a" (nick event) (name rule))
                     (remhash name-format (users-on-probation rule))
                     (case (rule-type rule)
                       (:KICK (irc:kick (channel event) (nick event) :reason message))
                       (:BAN (irc:channel-mode (channel event) "+b" :extra (nick event))
                             (irc:kick (channel event) (nick event) :reason message))
                       (:KILL (irc:kill (nick event) message))
                       (:MUTE (irc:channel-mode (channel event) "-v" :extra (nick event))
                              (respond event message))
                       (:SCOLD (respond event message))
                       (:REWARD (respond event message))
                       (T (respond event "WTF?"))))
                   (progn
                     (v:warn :rules "~a breached rule ~a" (nick event) (name rule))
                     (setf (gethash name-format (users-on-probation rule)) probation-count)
                     (respond event (rule-format event rule probation-count (breach-msg rule)))))))))))

(define-group rules)

(define-command (rules add) (name type &rest regex) (:authorization T :documentation "Add a new rule.")
  (if (gethash name (rules module))
      (respond event "A rule with that name already exists!")
      (progn
        (setf regex (format NIL "~{~a~^ ~}" regex)
              type (intern (string-upcase type) :KEYWORD))
        (assert (find type *types*) () "Type has to be one of ~{~a~^, ~}." *types*)
        (setf (gethash name (rules module))
              (make-instance
               'rule :name name
               :type type
               :regex regex
               :active-on (list (format NIL "~a/~a" (name (server event)) (channel event)))))
        (respond event "Added new rule ~a (~a) matching regex \"~a\"" name type regex))))

(define-command (rules remove) (name) (:authorization T :documentation "Remove a rule.")
  (if (gethash name (rules module))
      (progn
        (remhash name (rules module))
        (respond event "Rule removed: ~a" name))
      (respond event "A rule with that name does not exist!")))

(define-command (rules list) () (:documentation "List the current set of rules.")
  (respond event "Rules: ~{~a~^, ~}" (alexandria:hash-table-keys (rules module))))

(define-command (rules about) (name &optional field) (:documentation "See info about a rule.")
  (let ((rule (gethash name (rules module))))
    (if rule
        (if field
            (progn
              (setf field (find-symbol (string-upcase field)))
              (assert (find field *fields*) () "Field has to be one of ~{~a~^, ~}." *fields*)
              (respond event "~a: ~s" field (slot-value rule (find-symbol (format NIL "%~a" field)))))
            (respond event "~a TYPE: ~a REGEX: ~a PROBATION: ~a" name (rule-type rule) (regex rule) (probation rule)))
        (respond event "A rule with that name does not exist!"))))

(define-command (rules edit) (name field &rest value) (:authorization T :documentation "Change a setting of a rule.")
  (let ((rule (gethash name (rules module))))
    (if rule
        (progn
          (setf value (format NIL "~{~a~^ ~}" value)
                field (find-symbol (string-upcase field) :KEYWORD))
          (assert (and field (find field *fields*)) () "Field has to be one of ~{~a~^, ~}." *fields*)
          (cond
            ((eql field :TYPE)
             (setf value (find-symbol (string-upcase value) :KEYWORD))
             (assert (find value *types*) () "Type has to be one of ~{~a~^, ~}." *types*))
            ((eql field :PROBATION)
             (setf value (parse-integer value :junk-allowed T))
             (assert (not (null value)) () "Probation has to be an integer.")))
          (setf (slot-value rule (find-symbol (format NIL "%~a" field) :org.tymoonnext.colleen.mod.rules))
                value)
          (respond event "Rule ~a edited." name))
        (respond event "A rule with that name does not exist!"))))

(define-command (rules activate) (name &optional channel server) (:authorization T :documentation "Activate a rule for a channel.")
  (unless channel (setf channel (channel event)))
  (unless server (setf server (name (server event))))
  (let ((rule (gethash name (rules module))))
    (if rule
        (progn
          (pushnew (format NIL "~a/~a" server channel) (active-on rule) :test #'string-equal)
          (respond event "Rule ~a has been activated for ~a/~a" name server channel))
        (respond event "A rule with that name does not exist!"))))

(define-command (rules deactivate) (name &optional channel server) (:authorization T :documentation "Deactivate a rule for a channel.")
  (unless channel (setf channel (channel event)))
  (unless server (setf server (name (server event))))
  (let ((rule (gethash name (rules module))))
    (if rule
        (progn
          (setf (active-on rule)
                (delete (format NIL "~a/~a" server channel) (active-on rule) :test #'string-equal))
          (respond event "Rule ~a has been deactivated for ~a/~a" name server channel))
        (respond event "A rule with that name does not exist!"))))

(define-command (rules whitelisted) () (:documentation "Show a list of all whitelisted users.")
  (respond event "Whitelist: ~{~a~^, ~}" (whitelist module)))

(define-command (rules whitelist) (&optional nick server) (:authorization T :documentation "Add a user to the whitelist.")
  (unless nick (setf nick (nick event)))
  (unless server (setf server (name (server event))))
  (pushnew (format NIL "~a/~a" (string-upcase server) (string-upcase nick))
           (whitelist module) :test #'string-equal)
  (respond event "User ~a whitelisted!" nick))

(define-command (rules blacklist) (&optional nick server) (:authorization T :documentation "Remove a user from the whitelist.")
  (unless nick (setf nick (nick event)))
  (unless server (setf server (name (server event))))
  (setf (whitelist module)
        (delete (format NIL "~a/~a" (string-upcase server) (string-upcase nick))
                (whitelist module) :test #'string-equal))
  (respond event "User ~a removed from the whitelist!" nick))
