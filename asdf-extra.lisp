#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defclass module-system (asdf:system) ()
  (:documentation "ASDF system subclass for Colleen modules."))

(defmacro define-module-system (name components &optional depends-on)
  "Shorthand wrapper around ASDF:DEFSYSTEM to quickly define Colleen contrib modules."
  `(asdf:defsystem ,(intern (format NIL "CO-~a" name))
     :class :module-system
     :pathname ,(merge-pathnames "modules/" (asdf:system-source-directory :colleen))
     :name ,(format NIL "Colleen Module ~a" name)
     :serial T
     :components ,(mapcar #'(lambda (a) `(:file ,a)) components)
     :depends-on ,(cons :colleen depends-on)))

(defun load-module (name)
  "Tries to find and load a module of NAME."
  (let* ((name (string-downcase name))
         (system (or (ignore-errors (asdf:find-system (format NIL "co-~a" name)))
                     (ignore-errors (asdf:find-system name))))
         (load-fun (symbol-function
                    (or (and (find-package "QL") (find-symbol "QUICKLOAD" "QL"))
                        'asdf:load-system))))
    (if (and system (typep system 'module-system))
        (let ((name (asdf:component-name system)))
          (funcall load-fun name) name)
        (error 'module-system-not-found :name name))))

