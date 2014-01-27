#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.markov
  (:use :cl :colleen :events :alexandria)
  (:shadowing-import-from :colleen :restart))
(in-package :org.tymoonnext.colleen.mod.markov)

(defvar *registry-file* (merge-pathnames "markov-registry.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module markov ()
  ((%probability :initarg :probability :initform 25 :accessor probability)
   (%ignored-users :initarg :ignored-users :initform () :accessor ignored-users)
   (%active-in :initarg :active-in :initform () :accessor active-in)
   (%registry :initarg :registry :initform (make-hash-table :test 'equal) :accessor registry))
  (:documentation "Simple markov chain module."))

(defmethod start ((markov markov))
  (if (config-tree :markov :probability)
      (setf (probability markov) (config-tree :markov :probability)))
  (if (config-tree :markov :ignored-users)
      (setf (ignored-users markov) (config-tree :markov :ignored-users)))
  (if (config-tree :markov :active-in)
      (setf (active-in markov) (config-tree :markov :active-in)))
  (with-open-file (stream *registry-file* :if-does-not-exist NIL)
    (when stream
      (setf (registry markov) (yason:parse stream))
      (v:info :markov "Loaded ~a entries." (hash-table-count (registry markov)))))
  markov)

(defmethod stop ((markov markov))
  (setf (config-tree :markov :probability) (probability markov)
        (config-tree :markov :ignored-users) (ignored-users markov)
        (config-tree :markov :active-in) (active-in markov))
  (with-open-file (stream *registry-file* :direction :output :if-does-not-exist :create :if-exists :supersede)
    (cl-json:encode-json (registry markov) stream)
    (v:info :markov "Saved ~a entries." (hash-table-count (registry markov)))))

(define-handler (privmsg-event event) (:modulevar markov)
  (when (and (char= (aref (channel event) 0) #\#)
             (find (format NIL "~a/~a" (name (server event)) (channel event)) (active-in markov) :test #'string-equal))
    (unless (char= (aref (message event) 0) #\!)
      (learn markov (message event)))

    (when (< (random 100) (probability markov))
      (let ((wordlist (split-sequence:split-sequence #\Space (message event) :remove-empty-subseqs T)))
        (when (cdr wordlist)
          (let ((response (generate-string markov (first wordlist) (second wordlist))))
            (when (and response (not (string= (message event) response)))
              (respond event "~a" response))))))))

(define-group markov :documentation "Interact with the markov chain.")

(define-command (markov ignore) (&rest nicks) (:authorization T :documentation "Add users to the ignore list." :modulevar markov)
  (dolist (nick nicks)
    (pushnew nick (ignored-users markov) :test #'string-equal))
  (respond event "Users have been put on the ignore list."))

(define-command (markov unignore) (&rest nicks) (:authorization T :documentation "Remove users from the ignore list." :modulevar markov)
  (setf (ignored-users markov) 
        (delete-if #'(lambda (nick) (find nick nicks :test #'string-equal)) (ignored-users markov)))
  (respond event "Users have been removed from the ignore list."))

(define-command (markov list-ignored) () (:authorization T :documentation "List all ignored users." :modulevar markov)
  (respond event "Ignored users: ~:[None~;~:*~{~a~^, ~}~]" (ignored-users markov)))

(define-command (markov activate) (&optional channel server) (:authorization T :documentation "Activate markov learning and spouting for a channel." :modulevar markov)
  (let ((name (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
    (pushnew name (active-in markov) :test #'string-equal)
    (respond event "Learning/Spouting now activated on ~a." name)))

(define-command (markov deactivate) (&optional channel server) (:authorization T :documentation "Deactivate markov learning and spouting for a channel." :modulevar markov)
  (let ((name (format NIL "~a/~a" (or server (name (server event))) (or channel (channel event)))))
    (setf (active-in markov)
          (delete name (active-in markov) :test #'string-equal))
    (respond event "No longer learning/spouting on ~a." name)))

(define-command (markov probability) (&optional new-value) (:authorization T :documentation "Set or view the probability of invoking markov." :modulevar markov)
  (when new-value
    (setf (probability markov) (parse-integer new-value :junk-allowed T)))
  (respond event "Probability: ~a" (probability markov)))

(define-command (markov say) (&optional arg1 arg2) (:documentation "Let the bot say something." :modulevar markov)
  (if (and arg1 (not arg2)) (setf arg2 arg1 arg1 "!NONWORD!"))
  (let ((message (generate-string markov (or arg1 "!NONWORD!") (or arg2 "!NONWORD!"))))
    (if (and message (> (length message) 1))
        (respond event "~a" message)
        (respond event (fstd-message event :markov-nothing)))))

(defmethod learn ((markov markov) message)
  (let ((wordlist (split-sequence:split-sequence #\Space message :remove-empty-subseqs T)))
    (when (cddr wordlist)
      (v:debug :markov "Learning from: ~a" message)
      (loop for word1 = "!NONWORD!" then word2
         for word2 = "!NONWORD!" then word3
         for k = (format NIL "~a ~a" word1 word2)
         for word3 in wordlist
         do (push word3 (gethash k (registry markov)))
         finally (push "!NONWORD!" (gethash k (registry markov)))))))

(defgeneric generate-string (markov &optional word1 word2))
(defmethod generate-string ((markov markov) &optional (word1 "!NONWORD!") (word2 "!NONWORD!"))
  (let* ((output (if (string= word1 "!NONWORD!") "" (format NIL "~a ~a" word1 word2))))
    (unless (string= word1 "!NONWORD!")
      (let ((wordlist (remove "!NONWORD!" (gethash output (registry markov)) :test #'string=)))
        (when wordlist
          (let ((word3 (random-elt wordlist)))
            (setf output (format NIL "~a ~a" output word3)
                  word1 word2
                  word2 word3)))))
    (loop for i from 0 below 50
       for wordlist = (gethash (format NIL "~a ~a" word1 word2) (registry markov))
       while wordlist
       for word3 = (random-elt wordlist)
       until (string= word3 "!NONWORD!")
       do (setf output (format NIL "~a ~a" output word3)
                word1 word2
                word2 word3))
    (setf output (string-trim '(#\Space #\Tab #\Return #\Linefeed) output))
    (v:trace :markov "Generated string: ~a" output)
    (if (> (length (split-sequence:split-sequence #\Space output)) 0) output)))

