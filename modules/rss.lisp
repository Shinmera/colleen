#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.rss
  (:nicknames :co-rss)
  (:use :cl :colleen :events :lquery))
(in-package :org.tymoonnext.colleen.mod.rss)

(defparameter *save-file* (merge-pathnames "rss-feed-save.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module rss ()
  ((%thread-id :initform NIL :accessor thread-id))
  (:documentation "Update about new RSS feed items."))

(defclass feed ()
  ((%name :initarg :name :initform (error "Name required.") :accessor name)
   (%url :initarg :url :initform (error "URL required.") :accessor url)
   (%report-to :initarg :report-to :initform () :accessor report-to)
   (%last-item :initarg :last-item :initform NIL :accessor last-item))
  (:documentation "Class representation of an RSS feed."))

(uc:define-serializer (feed feed T)
  (make-array 3 :initial-contents (list (name feed) (url feed) (report-to feed))))

(uc:define-deserializer (feed array T)
  (make-instance 'feed :name (aref array 0) :url (aref array 1) :report-to (aref array 2)))

(defmethod print-object ((feed feed) stream)
  (print-unreadable-object (feed stream :type T)
    (format stream "~a" (name feed))))

(defclass feed-item ()
  ((%title :initarg :title :accessor title)
   (%description :initarg :description :accessor description)
   (%link :initarg :link :accessor link)
   (%guid :initarg :guid :accessor guid)
   (%publish-date :initarg :publish-date :accessor publish-date))
  (:documentation "Class representation of an RSS feed item."))

(defmethod print-object ((item feed-item) stream)
  (print-unreadable-object (item stream :type T)
    (format stream "~a ~a~@[ ~a~]" (guid item) (title item) (publish-date item))))

(defmethod load-storage :after ((rss rss))
  (with-module-storage (rss)
    (unless (uc:config-tree :feeds)
      (setf (uc:config-tree :feeds)
            (make-hash-table :test 'equalp)))))

(defmethod start ((rss rss))
  (setf (thread-id rss)
        (with-module-thread (rss)
          (check-loop rss))))

(define-condition recheck (error)
  ((%event :initarg :event :initform NIL :reader event)))
(defmethod check-loop ((rss rss))
  (v:debug :rss "Starting check-loop.")
  (loop with startup = T
        while (active rss)
        do (with-module-storage (rss)
             (handler-case
                 (progn
                   (sleep (* 60 5))
                   (v:debug :rss "[Check-Loop] Checking all...")
                   (loop for feed being the hash-values of (uc:config-tree :feeds)
                         do (handler-case 
                                (let ((update (update feed)))
                                  (when (and update (not startup))
                                    (v:info :rss "~a New item: ~a" feed update)
                                    (dolist (channel (report-to feed))
                                      (irc:privmsg (cdr channel) 
                                                   (format NIL "[RSS UPDATE] ~a: ~a ~a" 
                                                           (name feed) (title update) (link update)) 
                                                   :server (get-server (car channel))))))
                              (error (err)
                                (v:warn :rss "<~a> Error in check-loop: ~a" feed err)))))
               (recheck (err) 
                 (v:warn :rss "[Check-Loop] Skipping whatever it is I'm doing and rechecking immediately.")
                 (when (event err)
                   (respond (event err) "Rechecking now...")))
               (error (err)
                 (v:warn :rss "Error in check-loop: ~a" err))))
       (setf startup NIL))
  (v:debug :rss "Leaving check-loop."))

(defmethod update ((feed feed))
  (v:debug :rss "~a updating." feed)
  (let ((newest (first (get-items feed :limit 1))))
    (unless (and (last-item feed) 
                 (string-equal (guid (last-item feed)) (guid newest))
                 (string-equal (link (last-item feed)) (link newest)))
      (setf (last-item feed) newest)
      newest)))

(defmethod get-items ((feed feed) &key limit)
  (let* ((lquery:*lquery-master-document*)
         (plump:*tag-dispatchers* ())
         (drakma:*text-content-types* (cons '("application" . "xml")
                                            (cons '("application" . "rss+xml")
                                                  (cons '("application" . "atom+xml")
                                                        drakma:*text-content-types*))))
         (data (drakma:http-request (url feed))))
    (unless (stringp data)
      (error "Feed data seems to be incompatible! (Are you sure it is an RSS feed?)"))
    ($ (initialize data))
    (append
     (loop for node across ($ "item")
           for i from 0
           while (or (not limit) (< i limit))
           collect (make-instance 'feed-item 
                                  :title (string-trim (format NIL "~% ") ($ node "title" (text) (node)))
                                  :description (string-trim (format NIL "~% ") ($ node "description" (text) (node)))
                                  :link ($ node "link" (text) (node))
                                  :guid ($ node "guid" (text) (node))
                                  :publish-date ($ node "pubDate" (text) (node))))
     ;; Atom
     (loop for node across ($ "entry")
           for i from 0
           while (or (not limit) (< i limit))
           collect (make-instance 'feed-item 
                                  :title (string-trim (format NIL "~% ") ($ node "title" (text) (node)))
                                  :description (string-trim (format NIL "~% ") ($ node "content" (text) (node)))
                                  :link ($ node "link" (attr :href) (node))
                                  :guid ($ node "id" (text) (node))
                                  :publish-date ($ node "updated" (text) (node)))))))

(define-group rss :documentation "Manage RSS feeds.")

(define-command (rss add) (name url &optional (report-here T)) (:authorization T :documentation "Add a new RSS feed to check for updates.")
  (when (string-equal report-here "nil") (setf report-here NIL))
  (if (uc:config-tree :feeds name)
      (respond event "A feed with name \"~a\" already exists!" name)
      (handler-case
          (let ((feed (make-instance 'feed :name name :url url :report-to (when report-here (list (cons (name (server event)) (channel event)))))))
            (update feed)
            (setf (uc:config-tree :feeds name) feed)
            (v:info :rss "Added feed: ~a" feed)
            (respond event "Feed ~a added!" name))
        (error (err)
          (v:warn :rss "Failed to add feed ~a (~a): ~a" name url err)
          (respond event "Failed to add feed ~a: ~a" name err)))))

(define-command (rss remove) (name) (:authorization T :documentation "Remove an RSS feed.")
  (if (uc:config-tree :feeds name)
      (progn 
        (remhash name (uc:config-tree :feeds))
        (respond event "Feed ~a removed!" name))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss about) (name) (:documentation "Show information about a feed.")
  (if (uc:config-tree :feeds name)
      (respond event "~a: ~a" name (url (uc:config-tree :feeds name)))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss list) () (:documentation "List the currently available feeds.")
  (respond event "Known feeds: ~{~a~^, ~}" (alexandria:hash-table-keys (uc:config-tree :feeds))))

(define-command (rss watch) (name) (:authorization T :documentation "Start watching a feed on this channel.")
  (if (uc:config-tree :feeds name)
      (progn
        (pushnew (cons (name (server event)) (channel event))
                 (report-to (uc:config-tree :feeds name)))
        (respond event "Now watching ~a on this channel." name))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss unwatch) (name) (:authorization T :documentation "Stop watching a feed on this channel.")
  (if (uc:config-tree :feeds name)
      (progn
        (setf (report-to (uc:config-tree :feeds name)) 
              (delete-if #'(lambda (el) (and (eql (name (server event)) (car el))
                                             (string-equal (channel event) (cdr el))))
                         (report-to (uc:config-tree :feeds name))))
        (respond event "No longer watching ~a on this channel." name))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss latest) (name) (:documentation "Get the latest feed item.")
  (if (uc:config-tree :feeds name)
      (let ((item (first (get-items (uc:config-tree :feeds name) :limit 1))))
        (respond event "~a: ~a ~a~@[ ~a~]" (nick event) (title item) (link item) (publish-date item)))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss recheck) () (:authorization T :documentation "Forces an immediate recheck.")
  (bt:interrupt-thread (module-thread module (thread-id module))
                       #'(lambda () (signal 'recheck :event event))))
