#|
This file is a part of Colleen
(c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.eval
  (:use :cl :colleen :events :alexandria)
  (:shadow :eval))
(in-package :org.tymoonnext.colleen.mod.eval)

(defun get-all-symbols (&optional package)
  "Gets all symbols within a package."
  (let ((lst ())
        (package (find-package package)))
    (do-all-symbols (s lst)
      (if package
          (when (eql (symbol-package s) package)
            (push s lst))
          (push s lst)))
    lst))

(defparameter *symbol-blacklist* '())

(define-module eval () ()
  (:documentation "Provides a safe eval function."))

(defun safe-read (string)
  (let ((*read-eval* NIL))
    (read-from-string string)))

(defun safe-eval (form)
  (dolist (fun (flatten form))
    (assert (or (not (symbolp fun))
                (not (find fun *symbol-blacklist*))) () "Symbol ~a is not in whitelist!" fun))
  (cl:eval form))

(define-command eval (&rest form) (:authorization T :documentation "Evaluate a lisp form.")
  (handler-case
      (respond event "~s" (safe-eval (safe-read (format NIL "~{~a~^ ~}" form))))
    (error (err)
      (respond event "ERROR: ~a" err))))
