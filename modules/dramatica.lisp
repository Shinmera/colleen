#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.dramatica
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.dramatica)

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

(define-command (ed recent-changes) (&optional start-or-stop) (:authorization T :documentation "Start or stop the recent-changes polling." :modulevar dramatica)
  (cond
    ((string-equal start-or-stop "start")
     (if (log-loop dramatica)
         (respond event "Polling is already activated! (TID ~a)" (log-loop dramatica))
         (progn 
           (start-log-loop dramatica)
           (respond event "Recent-changes polling activated."))))

    ((string-equal start-or-stop "stop")
     (if (log-loop dramatica)
         (progn
           (stop-log-loop dramatica)
           (respond event "Recent-changes polling deactivated."))
         (respond event "Polling is already deactivated!")))
    
    (T (respond event "Recent changes polling is ~:[deactivated~;activated ~:*(TID~a)~]" (log-loop dramatica)))))

(defun start-log-loop (dramatica)
  (v:info :dramatica.recentchanges "Starting log-loop.")
  (setf (log-running dramatica) T)
  (setf (log-loop dramatica) (with-module-thread dramatica
                               (wiki-log-loop dramatica))))

(defun stop-log-loop (dramatica)
  (v:info :dramatica.recentchanges "Stopping log-loop.")
  (let* ((threadid (log-loop dramatica))
         (thread (gethash threadid (threads dramatica))))
    (when (bordeaux-threads:thread-alive-p thread)
      (v:warn :dramatica.recentchanges "Log-loop did not terminate normally.")
      (bordeaux-threads:destroy-thread thread)
      (remhash threadid (threads dramatica))))
  (setf (log-running dramatica) NIL
        (log-loop dramatica) NIL)
  (v:info :dramatica.recentchanges "Log-loop stopped."))

(defun wiki-log-loop (dramatica)
  (let ((wiki:*wiki-api* (config-tree :dramatica :wiki :api))
        (latest-timestamp
          (multiple-value-bind (query timestamp) (wiki:recent-changes :limit 1 :type :log)
            (declare (ignore query)) timestamp))
        (last-id 0))
    (v:info :dramatica.recentchanges "Log-loop started with latest-timestamp ~a" latest-timestamp)
    (loop while (log-running dramatica)
          do (sleep 10)
             (v:debug :dramatica.recentchanges "Querying log.")
             (ignore-errors
              (handler-case
                  (let ((query (wiki:recent-changes :end latest-timestamp :limit 1000 :type :log :properties '(user comment loginfo timestamp title))))
                    (setf latest-timestamp (cdr (assoc :timestamp (first query))))
                    (v:trace :dramatica.recentchanges "Log return: ~a" query)
                    (dolist (log query)
                      (when (< last-id (cdr (assoc :logid log)))
                        (setf last-id (cdr (assoc :logid log)))
                        (v:info :dramatica.recentchanges "New log entry: ~a" log)
                        
                        (handle-log (cdr (assoc :logtype log))
                                    (cdr (assoc :title log))
                                    (cdr (assoc :comment log))))))
                (error (err)
                  (v:warn :dramatica.recentchanges "ERROR: ~a" err)
                  (wiki:login (config-tree :dramatica :wiki :user)
                              (config-tree :dramatica :wiki :pass))))))
    (v:info :dramatica.recentchanges "Leaving log-loop with last-id ~a" last-id))
  (setf (log-loop dramatica) NIL))

(defvar *ip-match* (cl-ppcre:create-scanner "(\\d{1,4}\\.){3}\\d{1,4}"))
(defun handle-log (logtype title comment)
  (cond
    ((string= logtype "block")
     (let ((page title))
       (if (cl-ppcre:scan *ip-match* page)
           (v:info :dramatica.handle-log "<~a> Ban message skipped due to IP block." page)
           (let ((page-content (wiki:page-get page)))
             (if (search "{{banned}}" page-content)
                 (v:warn :dramatica.handle-log "<~a> Ban message already set!" page)
                 (handler-case
                     (progn
                       (if (search "spam" comment :test #'char-equal)
                           (wiki:page-edit page "{{banned}}")
                           (wiki:page-prepend page "{{banned}}"))
                       (v:info :dramatica.handle-log "<~a> Ban page created." page))))))))
    
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
