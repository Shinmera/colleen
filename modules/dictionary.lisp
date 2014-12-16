#|
 This file is a part of Colleen
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.colleen)
(defpackage #:org.tymoonnext.colleen.mod.dictionary
  (:nicknames #:co-dictionary)
  (:use #:cl #:colleen #:events #:alexandria))
(in-package #:org.tymoonnext.colleen.mod.dictionary)

(define-module dictionary () ()
  (:documentation "A general purpose dictionary provider."))

(defmethod start ((dictionary dictionary))
  (unless (eq (hash-table-test (storage dictionary)) 'equalp)
    (setf (storage dictionary) (make-hash-table :test 'equalp))))

(defun wiki-lookup (term)
  (labels ((trunc-text (text)
             (let ((text (string-trim '(#\Newline #\Space #\Linefeed #\Return) text)))
               (if (> (length text) 200)
                   (format NIL "~a..." (subseq text 0 200))
                   text)))
           (parse-wiki-content (data)
             (trunc-text (first (cl-ppcre:split
                                 "\\n" (lquery:$ (initialize (cl-ppcre:regex-replace-all "xml:" data ""))
                                         "p:first-only" (text) (node)))))))
    (let* ((wiki:*wiki-api* "http://en.wikipedia.org/w/api.php")
           (title (cdr (assoc :TITLE (first (wiki:wiki-search term :limit 1 :what "text"))))))
      (when title
        (setf title (cl-ppcre:regex-replace-all " " title "_"))
        (let ((data (wiki:wiki-parse :page title :section 0)))
          (format NIL "~a [http://en.wikipedia.org/wiki/~a]"
                  (parse-wiki-content data) title))))))

(defun link-p (definition)
  (and definition (< 2 (length definition))
       (string= definition "=>" :end1 2)
       (string-trim " " (subseq definition 2))))

(defun about-term (module event term &optional target)
  (with-module-storage (module)
    (let* ((term (string-trim " " term))
           (definition (or (uc:config-tree term)
                           (wiki-lookup term)))
           (link (link-p definition)))
      (if link
          (setf definition (uc:config-tree link)
                term (format NIL "~s: ~s" term link))
          (setf term (format NIL "~s" term)))
      (if definition
          (respond event (format-message event (format NIL "~@[~a, look at ~]~a: ~a" target term definition)))
          (respond event (format-message event (format NIL "~a: Sorry, I don't know anything about ~a." (or target (nick event)) term)))))))

(defun define-term (module event term definition)
  (with-module-storage (module)
    (let ((term (string-trim " " term))
          (definition (string-trim " " definition))
          (old-definition (uc:config-tree term)))
      (when-let ((link (link-p definition)))
        (let ((definition (uc:config-tree link)))
          (when-let ((definition (and definition (link-p definition))))
            (respond event "Cannot link to a link. Linking to ~a instead." definition)
            (setf term definition))))
      (setf (uc:config-tree term) definition)
      (respond event "~:[New~;Old~] term ~:*~:[~;re~]defined." old-definition))))

(define-handler (privmsg-event event) ()
  (let ((pre (format NIL "(?i)^~a[:,]\\s+(.*)" (nick (server event)))))
    (cl-ppcre:register-groups-bind (message) (pre (message event))
      (or (cl-ppcre:register-groups-bind (NIL NIL NIL NIL term definition)
              ("(?i)^(remember|define)\\s*(this)?\\s*(about|on|for)?\\s*(\\[?([^:\\]]+)\\]?):(.*)" message)
            (define-term module event term definition)
            T)
          (cl-ppcre:register-groups-bind (NIL command)
              ("(?i)^(do|execute|exec):?\\s*(.*)" message)
            (relay-command event command)
            T)
          (cl-ppcre:register-groups-bind (NIL NIL person term)
              ("(?i)^(define|tell|explain)?\\s*((me|[^\\s]+)\\s*about)?\\s*(.+)" message)
            (if (and person (string-equal person "me"))
                (about-term module event term (nick event))
                (about-term module event term person)))))))

(define-group dictionary :documentation "Manage the general purpose dictionary.")

(define-command (dictionary about) (&rest term) (:documentation "Look up a term.")
  (about-term module event (format NIL "~{~a~^ ~}" term)))

(define-command (dictionary define) (&rest definition) (:documentation "Define a new term, a semicolon separates term name and definition.")
  (cl-ppcre:register-groups-bind (term definition) ("(.*?):(.*)" definition)
    (define-term module event term definition)))

(define-command (dictionary remove) (&rest term) (:documentation "Remove a term.")
  (let ((term (format NIL "~{~a~^ ~}" term)))
    (remhash term (storage module))
    (respond event "Term removed.")))

(define-command (dictionary size) () (:documentation "Returns the number of known terms.")
  (respond event "Dictionary size: ~d terms." (hash-table-count (storage module))))

(define-command (dictionary search) (&rest term) (:documentation "Search for matching terms.")
  (let ((term (format NIL "~{~a~^ ~}" term)))
    (if (< (length term) 1)
        (respond event "Search term too short.")
        (respond event "Matching terms: ~{~a~^, ~}"
                 (loop for item being the hash-keys of (storage module)
                       if (search term item :test #'equalp)
                         collect item)))))

(define-command (dictionary random) () (:documentation "Show a random entry from the dictionary.")
  (let ((key (random-elt (hash-table-keys (storage module)))))
    (about-term module event key)))

(define-command (dictionary link) (from to) (:documentation "Create a link from one term to another.")
  (define-term module event from (format NIL "=> ~a" to)))

(define-command (dictionary is-link) (term) (:documentation "If the term is a link, returns the original term or otherwise NIL.")
  (respond event "~a" (link-p (uc:config-tree term))))


