#|
This file is a part of Colleen
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.stands4
  (:nicknames #:co-stands4)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.stands4)

(define-module stands4 () ())

(defun synonym-fields (word &rest fields)
  (let* ((result (drakma:http-request "http://www.stands4.com/services/v2/syno.php"
                                      :parameters `(("uid" . ,(uc:config-tree :uid))
                                                    ("tokenid" . ,(uc:config-tree :token))
                                                    ("word" . ,word))))
         (results (lquery:$ (initialize result) "result")))
    (if (= 0 (length results))
        (values NIL NIL)
        (let ((thing (lquery:$ results (first))))
          (values (mapcar (lambda (field) (lquery:$ thing field (text) (node))) fields) T)))))

(defmacro define-single-field-lookup-func (command descriptor field &optional (documentation ""))
  `(define-command ,command (&rest term) (:documentation ,documentation)
    (let ((word (format NIL "~{~a~^ ~}" term)))
      (multiple-value-bind (fields found) (synonym-fields word "term" ,field)
        (if found
            (respond event "~a for ~s: ~a" ,descriptor (first fields) (second fields))
            (respond event "No results found for ~s." word))))))

(define-single-field-lookup-func synonyms "Synonyms" "synonyms" "Look up synonyms of a term.")
(define-single-field-lookup-func antonyms "Antonyms" "antonyms" "Look up antonyms of a term.")
(define-single-field-lookup-func definition "Definition" "definition" "Look up the definition of a term.")

(define-command word (&rest term) (:documentation "Look up stuff about a word.")
  (let ((word (format NIL "~{~a~^ ~}" term)))
    (multiple-value-bind (fields found) (synonym-fields word "term" "partofspeech" "definition" "example")
      (if found
          (destructuring-bind (term part definition example) fields
            (respond event "~s (~a): ~a. ~:[~;Example: ~a~]"
                     term part definition (string/= example "") example))
          (respond event "No results found for ~s." word)))))
