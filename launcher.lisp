#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defun startup ()
  "Start up the bot: Load config, start modules, connect to servers."
  (v:info :startup "Loading config...")
  (load-config)
  (v:info :startup "Starting modules...")
  (dolist (module (cons "core" (remove "core" (bot-config :startup :modules) :test #'string-equal)))
    (handler-case (progn (load-module module)
                         (start-module module))
      (module-system-not-found (err)
        (v:error :startup "System for module not found: ~a" (name err))
        (when (gethash (find-symbol module "KEYWORD") *bot-modules*)
          (v:info :startup "Found module class, starting without load.")
          (start-module module)))))
  (v:info :startup "Connecting to servers...")
  (mapc #'connect (bot-config :startup :servers))
  (v:info :startup "Done."))

(defun shutdown ()
  "Shut down the bot: Disconnect servers, stop modules, save config."
  (v:info :shutdown "Disconnecting servers...")
  (mapc #'disconnect (remove :null (loop for val being the hash-values of *servers* collect val) :key #'name))
  (v:info :shutdown "Stopping modules...")
  (mapc #'(lambda (module) (ignore-errors (stop-module module)))
        (remove-if-not #'active (remove :core (loop for key being the hash-keys of *bot-modules* collect key))))
  (v:info :shutdown "Stopping core...")
  (stop-module :core)
  (v:info :shutdown "Done."))
