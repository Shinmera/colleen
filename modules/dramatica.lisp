#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.dramatica
  (:use :cl :colleen)
  (:shadowing-import-from :colleen :restart))
(in-package :org.tymoonnext.colleen.mod.dramatica)

(ql:quickload :cl-wiki)
(setf wiki:*wiki-api* "http://encyclopediadramatica.se/api.php")

(define-module dramatica ()
    ((%log-running :initarg :log-running :initform NIL :accessor log-running)
     (%log-loop :initarg :log-loop :initform NIL :accessor log-loop))
  (:documentation "Module related to Encyclopedia Dramatica activities."))

(define-group ed dramatica :documentation "Interact or change ED settings.")

;;;;;;;;;;;; RECENT CHANGES

(define-command (ed recent-changes) dramatica (&optional start-or-stop) (:authorization T :documentation "Start or stop the recent-changes polling.")
  (cond
    ((string-equal start-or-stop "start")
     (if (log-loop dramatica)
         (respond event "Polling is already activated!")
         (progn 
           (start-log-loop dramatica)
           (respond event "Recent-changes polling activated."))))

    ((string-equal start-or-stop "stop")
     (if (log-loop dramatica)
         (progn
           (stop-log-loop dramatica)
           (respond event "Recent-changes polling deactivated."))
         (respond event "Polling is already deactivated!")))
    
    (T (respond event "Recent changes polling is ~:[deactivated~;activated~]" (log-loop dramatica)))))

(defun start-log-loop (dramatica)
  (v:info :dramatica.recentchanges "Starting log-loop.")
  (setf (log-running dramatica) T
        (log-loop dramatica) (bordeaux-threads:make-thread #'(lambda () (wiki-log-loop dramatica)))))

(defun stop-log-loop (dramatica)
  (v:info :dramatica.recentchanges "Stopping log-loop.")
  (setf (log-running dramatica) NIL)
  ;; Wait for timeout or terminate.
  (loop for i from 0 to 10
     while (and (log-loop dramatica)
                (bordeaux-threads:thread-alive-p (log-loop dramatica)))
     do (sleep 1)
     finally (progn 
               (when (and (>= i 10) (log-loop dramatica) (bordeaux-threads:thread-alive-p (log-loop dramatica))) 
                 (bordeaux-threads:destroy-thread (log-loop dramatica)))
               (setf (log-loop dramatica) NIL))))

(defun wiki-log-loop (dramatica)
  (let ((latest-timestamp
         (multiple-value-bind (query timestamp) (wiki:recent-changes :limit 1 :type :log)
           (declare (ignore query)) timestamp))
        (last-id 0))
    (loop while (log-running dramatica)
       do (sleep 10)
         (v:debug :dramatica.recentchanges "Querying log.")
         (handler-case
             (let ((query (wiki:recent-changes :end latest-timestamp :limit 1000 :type :log :properties '(user comment loginfo timestamp title))))
               (setf latest-timestamp (cdr (assoc :timestamp (first query))))
               (v:trace :dramatica.recentchanges "Log return: ~a" query)
               (dolist (log query)
                 (when (> (cdr (assoc :logid log)) last-id)
                   (setf last-id (cdr (assoc :logid log)))
                   (v:info :dramatica.recentchanges "New log entry: ~a" log)
                   
                   (handle-log (cdr (assoc :logtype log))
                               (cdr (assoc :title log))
                               (cdr (assoc :comment log)))))))))
  (setf (log-loop dramatica) NIL))

(defun handle-log (logtype title comment)
  (cond
    ((string= logtype "block")
     (let* ((page title)
            (page-content (wiki:page-get page)))
       (if (search "{{banned}}" page-content)
           (v:warn :dramatica.handle-log "<~a> Ban message already set!" page)
           (handler-case
               (progn
                 (if (search "spam" comment :test #'char-equal)
                     (wiki:page-edit page "{{banned}}")
                     (wiki:page-prepend page "{{banned}}"))
                 (v:info :dramatica.handle-log "<~a> Ban page created." page))))))
    
    ((string= logtype "newusers")
     (let* ((page (format NIL "User_Talk:~a" (subseq title 5)))
            (page-content (wiki:page-get page)))
       (if (search "{{welcome}}" page-content)
           (v:warn :dramatica.handle-log "<~a> Welcome message already set!" page)
           (progn
             (wiki:page-append page "{{welcome}} ~~~~")
             (v:info :dramatica.handle-log "<~a> Welcome page created." page)))))))

;;;;;;;;;;;; OTHER COMMANDS

(define-command (ed login) dramatica (username password) (:authorization T :documentation "Log in to the wiki.")
  )

