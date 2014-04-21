#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defclass command ()
  ((%pattern)
   (%arguments)
   (%handler-function)
   (%documentation))
  (:documentation ""))

(defmethod dispatch ((event command-event))
  )

(defun read-command (event)
  )

(set-handler-function :command-reader 'privmsg #'read-command :MAIN)
