#|
 This file is a part of Colleen
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.dictionary
  (:use :cl :colleen :events :alexandria))
(in-package :org.tymoonnext.colleen.mod.dictionary)

(defvar *save-file* (merge-pathnames "dictionary.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module dictionary ()
    ((%dictionary :accessor dictionary :initform (make-hash-table :test 'equalp)))
  (:documentation "A general purpose dictionary provider."))

(defmethod start ((dictionary dictionary)) (%load dictionary))
(defmethod stop ((dictionary dictionary)) (%save dictionary))

(defmethod %load ((module dictionary))
  (with-open-file (stream *save-file* :if-does-not-exist NIL)
    (when stream
      (let ((dictionary (yason:parse stream)))
        (loop for k being the hash-keys of dictionary
              for v being the hash-values of dictionary
              do (setf (gethash k (dictionary module)) v))))))

(defmethod %save ((module dictionary))
  (with-open-file (stream *save-file* :direction :output :if-does-not-exist :create :if-exists :supersede)
    (yason:encode (dictionary module) stream)))

(defun link-p (definition)
  (and definition (< 2 (length definition))
       (string= definition "=>" :end1 2)
       (string-trim " " (subseq definition 2))))

(defun about-term (module event term)
  (let* ((term (string-trim " " term))
         (definition (gethash term (dictionary module))))
    (when-let ((link (link-p definition)))
      (setf definition (gethash link (dictionary module))
            term (format NIL "~a: ~a" term link)))
    (respond event (format-message event (format NIL "~a: ~:[Unknown term.~;~:*~a~]" term definition)))))

(defun define-term (module event term definition)
  (let ((term (string-trim " " term))
        (definition (string-trim " " definition))
        (old-definition (gethash term (dictionary module))))
    (when-let ((link (link-p definition)))
      (let ((definition (gethash link (dictionary module))))
        (when-let ((definition (and definition (link-p definition))))
          (respond event "Cannot link to a link. Linking to ~a instead." definition)
          (setf term definition))))
    (setf (gethash term (dictionary module)) definition)
    (respond event "~:[New~;Old~] term ~:*~:[~;re~]defined." old-definition)))

(define-handler (privmsg-event event) ()
  (let ((regex (format NIL "^~a:? (define (.*?):(.*)|(tell me |tell |define |explain )?(about )?(.*))" (nick (server event)))))
    (cl-ppcre:register-groups-bind (clause to-define definition fill0 fill1 term) (regex (message event))
      (declare (ignore clause fill0 fill1))
      (if term
          (about-term module event term)
          (define-term module event to-define definition)))))

(define-group dictionary :documentation "Manage the general purpose dictionary.")

(define-command (dictionary about) (&rest term) (:documentation "Look up a term.")
  (about-term module event (format NIL "~{~a~^ ~}" term)))

(define-command (dictionary define) (&rest definition) (:documentation "Define a new term, a semicolon separates term name and definition.")
  (cl-ppcre:register-groups-bind (term definition) ("(.*?):(.*)" definition)
    (define-term module event term definition)))

(define-command (dictionary remove) (&rest term) (:documentation "Remove a term.")
  (let ((term (format NIL "~{~a~^ ~}" term)))
    (remhash term (dictionary module))
    (respond event "Term removed.")))

(define-command (dictionary size) () (:documentation "Returns the number of known terms.")
  (respond event "Dictionary size: ~d terms." (hash-table-count (dictionary module))))

(define-command (dictionary search) (&rest term) (:documentation "Search for matching terms.")
  (let ((term (format NIL "~{~a~^ ~}" term)))
    (if (< (length term) 1)
        (respond event "Search term too short.")
        (respond event "Matching terms: ~{~a~^, ~}"
                 (loop for item in (hash-table-keys (dictionary module))
                       if (search term item :test #'equalp)
                         collect item)))))

(define-command (dictionary random) () (:documentation "Show a random entry from the dictionary.")
  (let ((key (random-elt (hash-table-keys (dictionary module)))))
    (about-term module event key)))

(define-command (dictionary link) (from to) (:documentation "Create a link from one term to another.")
  (define-term module event from (format NIL "=> ~a" to)))

(define-command (dictionary is-link) (term) (:documentation "If the term is a link, returns the original term or otherwise NIL.")
  (respond event "~a" (link-p (gethash term (dictionary module)))))
