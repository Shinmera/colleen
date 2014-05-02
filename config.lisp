#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defun parse-json (stream)
  (flet ((object-key-fn (name)
           (let ((name (with-output-to-string (stream)
                         (loop for char across name
                               do (format stream "~:[~a~;-~a~]" (upper-case-p char) (char-upcase char))))))
             (intern name "KEYWORD"))))
    ;;(yason:parse stream :object-key-fn #'object-key-fn :object-as :alist)
    ))

(defun load-config (&optional (config-file *conf-file*))
  "(Re)load the static configuration."
  (when (not config-file)
    (setf config-file *default-conf-file*))

  (with-open-file (stream config-file :if-does-not-exist :ERROR)
    (setf *conf* (parse-json stream))
    (setf *conf-file* config-file)
    (v:info :colleen.main "Loaded config from ~a" config-file)))

(defun save-config (&optional (config-file *conf-file*))
  "Save the static configuration to file."
  (when (not config-file)
    (setf config-file *default-conf-file*))
  
  (with-open-file (stream config-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    ;;(yason:encode-alist *conf* stream)
    (v:info :colleen.main "Saved config to ~a" config-file)))

(defun config (setting &optional new-value (config-file *conf-file*))
  "Get or set configuration values."
  (when new-value
    (setf (cdr (assoc setting *conf*)) new-value)
    (save-config config-file))
  (cdr (assoc setting *conf*)))

(defmacro config-tree (&rest branches)
  "Retrieve a configuration value based on a branch."
  (labels ((ct (branches)
             (if branches
                 `(cdr (assoc ,(car branches) ,(ct (cdr branches))))
                 `*conf*)))
    (ct (reverse branches))))

(defsetf config config)

(defun server-config (server var)
  "Shorthand accessor for server configuration values, defaulting to the DEFAULT server if not available."
  (if (not (keywordp var))
      (setf var (intern (string-upcase var) "KEYWORD")))
  (assert (not (eq var :DEFAULT)) () "Cannot use :DEFAULT server.")
  (or (config-tree :servers server var)
      (config-tree :servers :default var)))
