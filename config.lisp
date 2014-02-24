#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defun load-config (&optional (config-file *conf-file*))
  "(Re)load the static configuration."
  (when (not config-file)
    (setf config-file (merge-pathnames
                       "colleen.json"
                       (merge-pathnames "config/" (asdf:system-source-directory :colleen)))))

  (with-open-file (file config-file :if-does-not-exist :ERROR)
    (setf *conf* (json:decode-json file))
    (setf *conf-file* config-file)
    (v:info :colleen.main "Loaded config from ~a" config-file)))

(defun save-config (&optional (config-file *conf-file*))
  "Save the static configuration to file."
  (when (not config-file)
    (setf config-file (merge-pathnames "bot-config.json" (asdf:system-source-directory :colleen))))
  
  (with-open-file (stream config-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (json:encode-json-alist *conf* stream)
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

(defun format-message (event message &rest other-replaces)
  "Format the given message, replacing $NICK$, $CHANNEL$, $SERVER$ and $MYSELF$ with the according values."
  (let ((replaces (append other-replaces
                          `(("\\$NICK\\$" ,(nick event))
                            ("\\$CHANNEL\\$" ,(channel event))
                            ("\\$SERVER\\$" ,(string (name (server event))))
                            ("\\$MYSELF\\$" ,(nick (server event)))))))
    (flet ((rep (search replace message)
             (cl-ppcre:regex-replace-all search message replace)))
      (loop for replace in replaces
         do (setf message (rep (car replace) (cdr replace) message))
         finally (return message)))))

(defun standard-message (msgsymbol &rest otherwise-format)
  "Returns the default message designated by the symbol or if provided the otherwise-formatted string."
  (or (config-tree :messages msgsymbol)
      (when otherwise-format (apply #'format NIL otherwise-format))))

(defun fstd-message (event msgsymbol &rest otherwise-format)
  "Shorthand for (format-message (standard-message ..))."
  (let ((msg (or (config-tree :messages msgsymbol)
                 (when otherwise-format (apply #'format NIL otherwise-format)))))
    (when msg (format-message event msg))))

