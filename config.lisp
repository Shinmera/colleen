#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defun load-config (&optional (config-file (merge-pathnames "bot-config.json" (asdf:system-source-directory :colleen))))
  "(Re)load the static configuration."
  (when (not config-file)
    (setf config-file (merge-pathnames "bot-config.json" (asdf:system-source-directory :colleen))))

  (with-open-file (file config-file :if-does-not-exist :ERROR)
    (setf *conf* (json:decode-json file))
    (setf *conf-file* config-file)
    (v:info :colleen.main "Loaded config from ~a" config-file)))

(defun config (setting &optional new-value (config-file *conf-file*))
  "Get or set configuration values."
  (when new-value
    (setf (cdr (assoc setting *conf*)) new-value)
    (with-open-file (file config-file :if-exists :SUPERSEDE)
      (json:encode-json *conf* file)
      (v:info :colleen.main "Saved config to ~a" config-file)))
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
  (if (not (keywordp var))
      (setf var (intern (string-upcase var) "KEYWORD")))
  (assert (not (eq var :DEFAULT)) () "Cannot use :DEFAULT server.")
  (or (config-tree :servers server var)
      (config-tree :servers :default var)))

(defun format-message (event message)
  (flet ((rep (search replace message)
           (cl-ppcre:regex-replace-all search message replace)))
    (rep "\\$NICK\\$" (nick event)
         (rep "\\$CHANNEL\\$" (channel event)
              (rep "\\$MYSELF\\$" (server-config (name (server event)) :nick)
                   message)))))

(defun standard-message (msgsymbol &rest otherwise-format)
  (or (config-tree :messages msgsymbol)
      (when otherwise-format (apply #'format NIL otherwise-format))))

(defun fstd-message (event msgsymbol &rest otherwise-format)
  (let ((msg (or (config-tree :messages msgsymbol)
                 (when otherwise-format (apply #'format NIL otherwise-format)))))
    (when msg (format-message event msg))))

