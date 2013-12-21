#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.rss
  (:use :cl :colleen :events :lquery))
(in-package :org.tymoonnext.colleen.mod.rss)

(defparameter *save-file* (merge-pathnames "rss-feed-save.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))

(define-module rss () 
  ((%feeds :initform (make-hash-table :test 'equalp) :accessor feeds))
  (:documentation "Update about new RSS feed items."))

(defclass feed ()
  ((%name :initarg :name :initform (error "Name required.") :accessor name)
   (%url :initarg :url :initform (error "URL required.") :accessor url)
   (%report-to :initarg :report-to :initform () :accessor report-to)
   (%last-item :initarg :last-item :initform NIL :accessor last-item))
  (:documentation "Class representation of an RSS feed."))

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

(defmethod start ((rss rss))
  (load-feeds rss)
  (with-module-thread rss
    (check-loop rss)))

(defmethod stop ((rss rss))
  (save-feeds rss))

(defmethod save-feeds ((rss rss))
  (v:info :rss "Saving feeds...")
  (with-open-file (stream *save-file* :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((data (loop for feed being the hash-values of (feeds rss)
                   collect (cons (if (keywordp (name feed)) 
                                     (name feed) 
                                     (intern (name feed) :KEYWORD))
                                 (list (cons :URL (url feed))
                                       (cons :REPORT (list (report-to feed))))))))
      (json:encode-json data stream)
      (v:info :rss "~a feeds saved." (hash-table-count (feeds rss))))))

(defmethod load-feeds ((rss rss))
  (v:info :rss "Loading feeds...")
  (with-open-file (stream *save-file* :if-does-not-exist NIL)
    (when stream
      (let ((feeds (loop with feeds = (make-hash-table :test 'equalp) 
                      for feed in (json:decode-json stream)
                      for name = (string-downcase (car feed))
                      for url = (cdr (assoc :URL (cdr feed)))
                      for report = (first (cdr (assoc :REPORT (cdr feed))))
                      do (setf (gethash name feeds) (make-instance 'feed :name name :url url :report-to report))
                      finally (return feeds))))
        (setf (feeds rss) feeds)
        (v:info :rss "~a feeds loaded." (hash-table-count feeds))))))

(define-condition recheck (error) ())
(defmethod check-loop ((rss rss))
  (v:debug :rss "Starting check-loop.")
  (loop with startup = T
     while (active rss)
     do (handler-case
            (progn
              (sleep (* 60 5))
              (v:info :rss "[Check-Loop] Checking all...")
              (loop for feed being the hash-values of (feeds rss)
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
            (declare (ignore err))
            (v:debug :rss "[Check-Loop] Skipping whatever it is I'm doing and rechecking immediately.")))
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
         (drakma:*text-content-types* (cons '("application" . "xml")
                                            (cons '("application" . "rss+xml")
                                                  drakma:*text-content-types*)))
         (data (drakma:http-request (url feed))))
    (unless (stringp data)
      (error "Feed data seems to be incompatible! (Are you sure it is an RSS feed?)"))
    ($ (initialize data :type :XML))
    (loop for node in ($ "item")
       for i from 0
       while (or (not limit) (< i limit))
       collect (make-instance 'feed-item 
                              :title ($ node "title" (text) (node))
                              :description ($ node "description" (text) (node))
                              :link ($ node "link" (text) (node))
                              :guid ($ node "guid" (text) (node))
                              :publish-date ($ node "pubDate" (text) (node))))))

(define-group rss :documentation "Manage RSS feeds.")

(define-command (rss add) (name url &optional (report-here T)) (:authorization T :documentation "Add a new RSS feed to check for updates.")
  (when (string-equal report-here "nil") (setf report-here NIL))
  (if (gethash name (feeds module))
      (respond event "A feed with name \"~a\" already exists!" name)
      (handler-case
          (let ((feed (make-instance 'feed :name name :url url :report-to (when report-here (list (cons (name (server event)) (channel event)))))))
            (update feed)
            (setf (gethash name (feeds module)) feed)
            (v:info :rss "Added feed: ~a" feed)
            (respond event "Feed ~a added!" name))
        (error (err)
          (v:warn :rss "Failed to add feed ~a (~a): ~a" name url err)
          (respond event "Failed to add feed ~a: ~a" name err)))))

(define-command (rss remove) (name) (:authorization T :documentation "Remove an RSS feed.")
  (if (gethash name (feeds module))
      (progn 
        (remhash name (feeds module))
        (respond event "Feed ~a removed!" name))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss about) (name) (:documentation "Show information about a feed.")
  (if (gethash name (feeds module))
      (respond event "~a: ~a" name (url (gethash name (feeds module))))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss list) () (:documentation "List the currently available feeds.")
  (respond event "Known feeds: ~{~a~^, ~}" (alexandria:hash-table-keys (feeds module))))

(define-command (rss watch) (name) (:authorization T :documentation "Start watching a feed on this channel.")
  (if (gethash name (feeds module))
      (progn
        (pushnew (cons (name (server event)) (channel event)) (report-to (gethash name (feeds module))))
        (respond event "Now watching ~a on this channel." name))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss unwatch) (name) (:authorization T :documentation "Stop watching a feed on this channel.")
  (if (gethash name (feeds module))
      (progn
        (setf (report-to (gethash name (feeds module))) 
              (delete-if #'(lambda (el) (and (eql (name (server event)) (car el))
                                             (string-equal (channel event) (cdr el))))
                                            (report-to module)))
        (respond event "No longer watching ~a on this channel." name))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss latest) (name) (:documentation "Get the latest feed item.")
  (if (gethash name (feeds module))
      (let ((item (first (get-items (gethash name (feeds module)) :limit 1))))
        (respond event "~a: ~a ~a~@[ ~a~]" (nick event) (title item) (link item) (publish-date item)))
      (respond event "No feed called \"~a\" could be found!" name)))

(define-command (rss recheck) () (:authorization T :documentation "Forces a recheck of all feeds immediately.")
  (bordeaux-threads:interrupt-thread 
   (thread module)
   #'(lambda () (error 'recheck)))
  (respond event "Forcing recheck..."))
