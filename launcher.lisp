#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defun startup ()
  (v:info :colleen.main "Starting up...")
  (setf *shutting-down* NIL)
  ;; Configuration
  (load-config)
  ;; Connect
  (connect)
  (reconnect-loop))

(defun shutdown ()
  (v:info :colleen.main "Shutting down...")
  (setf *shutting-down* T)
  (disconnect)
  (setf *shutting-down* NIL)
  (v:info :colleen.main "Shut down complete."))

(define-condition restart-error () ())
(define-condition shutdown-error () ())

(defun restart-loop ()
  (handler-case
      (handler-bind
          ((restart-error #'(lambda (c) 
                          (declare (ignore c))
                          (shutdown)
                          (invoke-restart 'restart-bot)))
           (error #'(lambda (c)
                      (v:error :colleen.launcher "Caught error on restart-level: ~a" c)
                      (if *on-error-continue*
                          (invoke-restart 'continue)
                          (error c)))))                          
        (restart-case (startup)
          (restart-bot () (restart-loop))))
    (shutdown-error () (shutdown))))

(defun main ()
  (setf *read-thread* 
        (bordeaux-threads:make-thread 
         (lambda ()
           (restart-loop)))))
