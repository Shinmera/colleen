#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defvar *config-file* NIL "Pathname pointing to the config file being used.")
(defvar *config-directory* (merge-pathnames "config/" (asdf:system-source-directory :colleen))
  "The directory pathname where configuration files are stored.")
(defvar *default-config-file* (merge-pathnames "colleen.uc.lisp" *config-directory*)
  "Pathname pointing to the default colleen central configuration file.")
(defvar *sample-config-file* (merge-pathnames "sample.uc.lisp" (asdf:system-source-directory :colleen))
  "Pathname pointing to the sample colleen central configuration file.")
(defvar *config* NIL "Bot core config.")

(defun load-config (&optional (config-file *config-file*))
  "(Re)load the static configuration."
  (when (not config-file)
    (setf config-file *default-config-file*)
    (ensure-directories-exist config-file))
  (setf *config-file* config-file)

  (unless (probe-file config-file)
    (v:warn :colleen.main "Falling back to sample config!")
    (setf config-file *sample-config-file*))
  
  (let ((uc:*config*))
    (uc:load-configuration config-file)
    (setf *config* uc:*config*)
    (v:info :colleen.main "Loaded config from ~a" config-file)))

(defun save-config (&optional (config-file *config-file*))
  "Save the static configuration to file."
  (when (not config-file)
    (setf config-file *default-config-file*))

  (uc:save-configuration config-file :object *config*)
  (v:info :colleen.main "Saved config to ~a" config-file))

(defun bot-config (&rest accessors)
  "Wrapper around UC:CONFIG-TREE that always uses the colleen central config."
  (let ((uc:*config* *config*))
    (apply #'uc:config-tree accessors)))

(defun (setf bot-config) (value &rest accessors)
  (let ((uc:*config* *config*))
    (uc:set-config-tree accessors value)))

(defun server-config (server var)
  "Shorthand accessor for server configuration values, defaulting to the DEFAULT server if not available."
  (if (not (keywordp var))
      (setf var (intern (string-upcase var) "KEYWORD")))
  (assert (not (eq var :DEFAULT)) () "Cannot use :DEFAULT server.")
  (or (bot-config :servers server var)
      (bot-config :servers :default var)))
