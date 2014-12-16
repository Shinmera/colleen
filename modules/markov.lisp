#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.markov
  (:nicknames #:co-markov)
  (:use #:cl #:colleen #:events #:alexandria)
  (:export #:learn #:generate-string))
(in-package #:org.shirakumo.colleen.mod.markov)

(defvar *registry-file* (merge-pathnames "markov.registry.uc.lisp" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module markov ()
  ((%registry :initarg :registry :initform (make-hash-table :test 'equal) :accessor registry))
  (:documentation "Simple markov chain module."))

(defmethod load-storage :after ((markov markov))
  (let ((uc:*config*))
    (if (uc:load-configuration *registry-file* :if-does-not-exist NIL)
        (setf (registry markov) uc:*config*)
        (setf (registry markov) (make-hash-table :test 'equalp))))
  markov)

(defmethod save-storage :after ((markov markov))
  (uc:save-configuration *registry-file* :object (registry markov)))

(define-handler (privmsg-event event) (:modulevar markov)
  (when (and (char= (aref (channel event) 0) #\#)
             (find (format NIL "~a/~a" (name (server event)) (channel event)) (uc:config-tree :active-in) :test #'string-equal))
    (unless (char= (aref (message event) 0) #\!)
      (learn (message event)))

    (when (< (random 100) (or (uc:config-tree :probability) 0))
      (let ((wordlist (split-sequence:split-sequence #\Space (message event) :remove-empty-subseqs T)))
        (when (cdr wordlist)
          (let ((response (generate-string (first wordlist) (second wordlist))))
            (when (and response (not (string= (message event) response)))
              (respond event "~a" response))))))))

(define-group markov :documentation "Interact with the markov chain.")

(define-command (markov ignore) (&rest nicks) (:authorization T :documentation "Add users to the ignore list." :modulevar markov)
  (dolist (nick nicks)
    (pushnew nick (uc:config-tree :ignored-users) :test #'string-equal))
  (respond event "Users have been put on the ignore list."))

(define-command (markov unignore) (&rest nicks) (:authorization T :documentation "Remove users from the ignore list." :modulevar markov)
  (setf (uc:config-tree :ignored-users)
        (delete-if #'(lambda (nick) (find nick nicks :test #'string-equal)) (uc:config-tree :ignored-users)))
  (respond event "Users have been removed from the ignore list."))

(define-command (markov list-ignored) () (:authorization T :documentation "List all ignored users." :modulevar markov)
  (respond event "Ignored users: ~:[None~;~:*~{~a~^, ~}~]" (ignored-users markov)))

(define-command (markov activate) (&optional channel server) (:authorization T :documentation "Activate markov learning and spouting for a channel." :modulevar markov)
  (let ((name (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
    (pushnew name (uc:config-tree :active-in) :test #'string-equal)
    (respond event "Learning/Spouting now activated on ~a." name)))

(define-command (markov deactivate) (&optional channel server) (:authorization T :documentation "Deactivate markov learning and spouting for a channel." :modulevar markov)
  (let ((name (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
    (setf (uc:config-tree :active-in)
          (delete name (uc:config-tree :active-in) :test #'string-equal))
    (respond event "No longer learning/spouting on ~a." name)))

(define-command (markov probability) (&optional new-value) (:authorization T :documentation "Set or view the probability of invoking markov." :modulevar markov)
  (when new-value
    (setf (uc:config-tree :probability) (parse-integer new-value :junk-allowed T)))
  (respond event "Probability: ~a" (uc:config-tree :probability)))

(define-command (markov say) (&optional arg1 arg2) (:documentation "Let the bot say something." :modulevar markov)
  (if (and arg1 (not arg2)) (setf arg2 arg1 arg1 "!NONWORD!"))
  (let ((message (generate-string (or arg1 "!NONWORD!") (or arg2 "!NONWORD!"))))
    (if (and message (> (length message) 1))
        (respond event "~a" message)
        (respond event (fstd-message event :markov-nothing)))))

(define-command (markov registry) () (:documentation "Report the markov registry size." :modulevar markov)
  (respond event "Markov registry size: ~,,'':d" (hash-table-count (registry markov))))

(defun learn (message)
  (let ((registry (registry (module :markov)))
        (wordlist (split-sequence:split-sequence #\Space message :remove-empty-subseqs T)))
    (when (cddr wordlist)
      (v:debug :markov "Learning from: ~a" message)
      (loop for word1 = "!NONWORD!" then word2
         for word2 = "!NONWORD!" then word3
         for k = (format NIL "~a ~a" word1 word2)
         for word3 in wordlist
         do (push word3 (gethash k registry))
         finally (push "!NONWORD!" (gethash k registry))))))

(defun generate-string (&optional (word1 "!NONWORD!") (word2 "!NONWORD!"))
  (let ((registry (registry (module :markov)))
        (output (if (string= word1 "!NONWORD!") "" (format NIL "~a ~a" word1 word2))))
    (unless (string= word1 "!NONWORD!")
      (let ((wordlist (remove "!NONWORD!" (gethash output registry) :test #'string=)))
        (when wordlist
          (let ((word3 (random-elt wordlist)))
            (setf output (format NIL "~a ~a" output word3)
                  word1 word2
                  word2 word3)))))
    (loop for i from 0 below 50
       for wordlist = (gethash (format NIL "~a ~a" word1 word2) registry)
       while wordlist
       for word3 = (random-elt wordlist)
       until (string= word3 "!NONWORD!")
       do (setf output (format NIL "~a ~a" output word3)
                word1 word2
                word2 word3))
    (setf output (string-trim '(#\Space #\Tab #\Return #\Linefeed) output))
    (v:trace :markov "Generated string: ~a" output)
    (if (> (length (split-sequence:split-sequence #\Space output)) 0) output)))
