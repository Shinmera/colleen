#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.dramatica
  (:use :cl :colleen :events)
  (:shadowing-import-from :colleen :restart))
(in-package :org.tymoonnext.colleen.mod.dramatica)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-wiki)
  (ql:quickload :xencl))

(define-module dramatica ()
    ((%log-running :initarg :log-running :initform NIL :accessor log-running)
     (%log-loop :initarg :log-loop :initform NIL :accessor log-loop))
  (:documentation "Module related to Encyclopedia Dramatica activities."))

(defmethod start ((dramatica dramatica))
  (setf wiki:*wiki-api* (config-tree :dramatica :wiki :api))
  (when (> (length (config-tree :dramatica :wiki :pass)) 0)
    (wiki:login (config-tree :dramatica :wiki :user)
                (config-tree :dramatica :wiki :pass)))
  (when (> (length (config-tree :dramatica :forum :pass)) 0)
    (xencl:initiate (config-tree :dramatica :forum :url)
                    (config-tree :dramatica :forum :user)
                    (config-tree :dramatica :forum :pass))))

(defmethod stop ((dramatica dramatica))
  (setf (config-tree :dramatica :wiki :api) wiki:*wiki-api*)
  (setf (config-tree :dramatica :forum :url) xencl:*index*))

;;;;;;;;;;;; RECENT CHANGES
(define-group ed :documentation "Interact or change ED settings.")

(define-command (ed recent-changes) (&optional start-or-stop) (:authorization T :documentation "Start or stop the recent-changes polling.")
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

;;;;;;;;;;;; WIKI COMMANDS

(define-group wiki :documentation "Commands for the Encyclopedia Dramatica Wiki.")

(define-command (wiki api-page) (&optional new-url) (:authorization T :documentation "Change or view the used MediaWiki API URL.")
  (when new-url
    (setf wiki:*wiki-api* new-url))
  (respond event "Used API: ~a" wiki:*wiki-api*))

(define-command (wiki login) (&optional username password) (:authorization T :documentation "Log in to the wiki.")
  (handler-case
      (progn
        (wiki:login (or username (config-tree :dramatica :wiki :user)) 
                    (or password (config-tree :dramatica :wiki :pass)))
        (respond event "Login successful."))
    (error (err)
      (respond event "Error: ~a" err))))

(defmacro define-wiki-command (name (&rest args) (&rest options) &body body)
  `(define-command (wiki ,name) ,args ,options
     (handler-case
         (progn ,@body)
       (wiki:wiki-error (err)
         (respond event "Wiki error: ~a (E~a)" (wiki:info err) (wiki:code err))))))

(define-wiki-command token () (:authorization T :documentation "Retrieve the edit-token.")
  (respond event "Token: ~a" (wiki:token)))

(define-command page (page) (:documentation "Retrieve the contents of a page.")
  (respond event (wiki:page-get page)))

(define-command prepend (page &rest content) (:authorization T :documentation "Prepend content to a page.")
  (wiki:page-prepend page (format NIL "~{~a~^ ~}" content))
  (respond event "Page edited."))

(define-command append (page &rest content) (:authorization T :documentation "Append content to a page.")
  (wiki:page-append page (format NIL "~{~a~^ ~}" content))
  (respond event "Page edited."))

(define-command create (page &rest content) (:authorization T :documentation "Create a new page.")
  (wiki:page-create page (format NIL "~{~a~^ ~}" content))
  (respond event "Page created."))

(define-command edit (page &rest content) (:authorization T :documentation "Edit the content of a page.")
  (wiki:page-edit page (format NIL "~{~a~^ ~}" content))
  (respond event "Page edited."))

(define-command delete (page) (:authorization T :documentation "Delete a page.")
  (wiki:page-delete page)
  (respond event "Page deleted."))

(define-command protect (page &optional (expiry "never") cascade &rest reason) (:authorization T :documentation "Protect a page from edits.")
  (setf cascade (string-equal cascade "yes")
        reason (format NIL "~{~a~^ ~}" reason))
  (wiki:page-protect page :expiry expiry :cascade cascade :reason reason)
  (respond event "Page protected until ~a (~a)~@[~* cascading the protect~]." expiry reason cascade))

(define-command rollback (page &rest edit-summary) (:authorization T :documentation "Roll a page back to the previous change.")
  (wiki:page-rollback page :summary (format NIL "~{~a~^ ~}" edit-summary))
  (respond event "Page rollback performed."))

(define-command block (user &optional (expiry "never") &rest reason) (:authorization T :documentation "Block a user from the wiki.")
  (setf reason (format NIL "~{~a~^ ~}" reason))
  (wiki:user-block user :expiry expiry :reason reason)
  (respond event "User blocked until ~a (~a)." expiry reason))

;;;;;;;;;;;; FORUM COMMANDS

(define-group edf :documentation "Commands for the Encyclopedia Dramatica Forums.")

(define-command (edf url) (&optional url) (:authorization T :documentation "View or set the forum URL.")
  (when url 
    (setf xencl:*index* url))
  (respond event "Used URL: ~a" xencl:*index*))

(defmacro define-edf-command (name (&rest args) (&rest options) &body body)
  `(define-command (edf ,name) ,args ,options
     (handler-case
         (progn ,@body)
       (xencl:forum-error (err)
         (respond event "Forum error: ~a (E~a) ~@[on page ~a~]" (xencl:info err) (xencl:code err) (xencl:page err))))))

(define-edf-command login (&optional username password) (:authorization T :documentation "Log in to the forum.")
  (xencl:login (make-instance 'xencl:user
                              :title (or username (config-tree :dramatica :forum :user))
                              :pass (or password (config-tree :dramatica :forum :pass))))
  (respond event "Login successful."))

(define-edf-command shoutbox-post (&rest message) (:authorization T :documentation "Posted a message to the shoutbox.")
  (xencl:post (make-instance 'xencl:shoutbox) (format NIL "~{~a~^ ~}" message))
  (respond event "Message posted."))
