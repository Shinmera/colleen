#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defun startup ()
  (v:info :startup "Loading config...")
  (load-config)
  (v:info :startup "Starting modules...")
  (apply #'start-module (config-tree :startup :modules))
  (v:info :startup "Connecting to servers...")
  (mapc #'connect (config-tree :startup :servers))
  (v:info :startup "Done."))

(defun shutdown ()
  (v:info :shutdown "Disconnecting servers...")
  (mapc #'disconnect (alexandria:hash-table-values *servers*))
  (v:info :shutdown "Stopping modules...")
  (mapc #'(lambda (module) (ignore-errors (stop-module module))) (alexandria:hash-table-keys *bot-modules*))
  (v:info :shutdown "Saving config...")
  (save-config)
  (v:info :shutdown "Done."))
