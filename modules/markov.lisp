#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.markov
  (:use :cl :colleen :alexandria)
  (:shadowing-import-from :colleen :restart))
(in-package :org.tymoonnext.colleen.mod.markov)

(defvar *registry-file* (merge-pathnames "markov-registry.json" (asdf:system-source-directory :colleen)))

(define-module markov ()
  ((%probability :initarg :probability :initform 25 :accessor probability)
   (%ignored-users :initarg :ignored-users :initform () :accessor ignored-users)
   (%registry :initarg :registry :initform (make-hash-table :test 'equal) :accessor registry)))

(defmethod start ((markov markov))
  (if (config-tree :markov :probability)
      (setf (probability markov) (config-tree :markov :probability)))
  (if (config-tree :markov :ignored-users)
      (setf (ignored-users markov) (config-tree :markov :ignored-users)))
  (with-open-file (stream *registry-file* :if-does-not-exist NIL)
    (when stream
      (setf (registry markov) (yason:parse stream))))
  markov)

(defmethod stop ((markov markov))
  (setf (config-tree :markov :probability) (probability markov)
        (config-tree :markov :ignored-users) (ignored-users markov))
  (with-open-file (stream *registry-file* :direction :output :if-does-not-exist :create :if-exists :supersede)
    (cl-json:encode-json (registry markov) stream)))

(define-handler on-message markov (privmsg-event event)
  (when (char= (aref (channel event) 0) #\#)
    (learn markov (message event))

    (when (< (random 100) (probability markov))
      (let ((wordlist (split-sequence:split-sequence #\Space (message event) :remove-empty-subseqs T)))
        (when (cdr wordlist)
          (let ((response (generate-string markov (first wordlist) (second wordlist))))
            (when (and response (not (string= (message event) response)))
              (respond event response))))))))

(define-command markov markov (&optional action &rest args) (:authorization T)
  (cond
    ((string-equal action "ignore")
     (pushnew (first args) (ignored-users markov) :test #'string-equal)
     (respond event "User ~a has been put on the ignore list." (first args)))
    ((string-equal action "unignore")
     (setf (ignored-users markov) (delete (first args) (ignored-users markov) :test #'string-equal))
     (respond event "User ~a has been removed from the ignore list." (first args)))
    ((string-equal action "list-ignored")
     (respond event "Ignored users: ~:[None~;~:*~{~a~^, ~}~]" (ignored-users markov)))
    ((string-equal action "probability")
     (setf (probability markov) (parse-integer (first args) :junk-allowed T))
     (respond event "Probability set to ~a" (probability markov)))
    ((string-equal action "say")
     (let ((message (generate-string markov (first args) (second args))))
       (if message
           (respond event message)
           (respond event (fstd-message event :markov-nothing)))))
    (T
     (respond event "Possible commands: ignore <nick>, unignore <nick>, list-ignored, probability <n>, say [seed1] [seed2]"))))

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
  (let* ((output (if (eq word1 "!NONWORD!") "" (format NIL "~a ~a" word1 word2))))
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
