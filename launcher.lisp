#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defun startup ()
  "Start up the bot: Load config, start modules, connect to servers."
  (v:info :startup "Loading config...")
  (load-config)
  (v:info :startup "Starting modules...")
  (apply #'start-module (config-tree :startup :modules))
  (v:info :startup "Connecting to servers...")
  (mapc #'connect (config-tree :startup :servers))
  (v:info :startup "Done."))

(defun shutdown ()
  "Shut down the bot: Disconnect servers, stop modules, save config."
  (v:info :shutdown "Disconnecting servers...")
  (mapc #'disconnect (alexandria:hash-table-values *servers*))
  (v:info :shutdown "Stopping modules...")
  (mapc #'(lambda (module) (ignore-errors (stop-module module)))
        (remove-if-not #'active (remove :core (hash-table-keys *bot-modules*))))
  (v:info :shutdown "Stopping core...")
  (stop-module :core)
  (v:info :shutdown "Done."))
