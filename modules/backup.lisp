#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)
(defpackage #:org.shirakumo.colleen.mod.backup
  (:nicknames #:co-backup)
  (:use #:cl #:colleen #:events))
(in-package #:org.shirakumo.colleen.mod.backup)

(define-module backup ()
    ((%directory :initform NIL :accessor backup-directory))
  (:documentation "Automatic periodic configuration backup module."))

(defmethod load-storage :after ((backup backup))
  (with-module-storage (backup)
    (setf (backup-directory backup) (parse-namestring (or (uc:config-tree :directory)
                                                          (namestring (merge-pathnames "backup/"
                                                                                       (asdf:system-source-directory :colleen))))))))

(defmethod save-storage :before ((backup backup))
  (with-module-storage (backup)
    (setf (uc:config-tree :directory) (namestring (backup-directory backup)))))

(defmethod start ((backup backup))
  (with-module-storage (backup)
    (reschedule (or (uc:config-tree :interval) (* 60 60 24)))))

(defun backup ()
  (let* ((directory (backup-directory (get-module :backup)))
         (timestamp (local-time:format-timestring NIL (local-time:now) :format '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2))))
         (target-dir (merge-pathnames (format NIL "~a/" timestamp) directory))
         (running-modules (remove-if-not #'active (remove (get-module :backup) (alexandria:hash-table-values *bot-modules*)))))
    (flet ((copy-fun (pathname)
             (let ((target-path (merge-pathnames (subseq (namestring pathname) (length (namestring *config-directory*))) target-dir)))
               (if (cl-fad:directory-pathname-p pathname)
                   (progn
                     (v:debug :backup "Ensuring directory ~a" target-path)
                     (ensure-directories-exist target-path))
                   (progn
                     (v:debug :backup "Copying file ~a" pathname)
                     (cl-fad:copy-file pathname target-path))))))
      (v:info :backup "Starting backup.")
      (v:debug :backup "Saving storage of all active modules...")
      (dolist (module running-modules)
        (handler-case
            (save-storage module)
          (error (err)
            (v:error :backup "Error saving storage for module ~a: ~a" module err))))
      (v:debug :backup "Copying folder ~a to ~a..." (namestring *config-directory*) (namestring target-dir))
      (copy-fun *config-directory*)
      (cl-fad:walk-directory *config-directory* #'copy-fun :directories T)
      (v:info :backup "Backup complete."))))

(defun restore (backup-name)
  (let* ((directory (backup-directory (get-module :backup)))
         (source-dir (merge-pathnames (format NIL "~a/" backup-name) directory))
         (running-modules (remove-if-not #'active (remove (get-module :backup) (alexandria:hash-table-values *bot-modules*)))))
    (assert (cl-fad:file-exists-p source-dir) () "Backup ~a does not exist!" backup-name)
    (flet ((copy-fun (pathname)
             (let ((target-path (merge-pathnames (subseq (namestring pathname) (length (namestring source-dir))) *config-directory*)))
               (if (cl-fad:directory-pathname-p pathname)
                   (progn
                     (v:debug :backup "Ensuring directory ~a" target-path)
                     (ensure-directories-exist target-path))
                   (progn
                     (v:debug :backup "Copying file ~a" pathname)
                     (cl-fad:copy-file pathname target-path))))))
      (v:info :backup "Starting restore.")
      (v:debug :backup "Deleting old config folder...")
      (cl-fad:delete-directory-and-files *config-directory* :if-does-not-exist :ignore)
      (v:debug :backup "Copying folder ~a to ~a..." (namestring source-dir) (namestring *config-directory*))
      (copy-fun source-dir)
      (cl-fad:walk-directory source-dir #'copy-fun :directories T)
      (v:debug :backup "Loading storage of all active modules...")
      (dolist (module running-modules)
        (handler-case
            (load-storage module)
          (error (err)
            (v:error :backup "Error loading storage for module ~a: ~a" module err))))
      (v:info :backup "Restore complete."))))

(defun last-backup (backup)
  (let ((backups (sort (remove-if-not #'cl-fad:directory-pathname-p (cl-fad:list-directory (backup-directory backup)))
                       #'(lambda (a b) (string> (namestring a) (namestring b))))))
    (car (last (pathname-directory (first backups))))))

(defun format-time-since (secs)
  (multiple-value-bind (s m h dd yy) (decode-universal-time secs)
    (setf yy (- yy 1) dd (- dd 1) h (- h 1))
    (format NIL "~@[~D years ~]~@[~D days ~]~@[~D hours ~]~@[~D minutes ~]~@[~D seconds~]"
            (unless (= yy 0) yy) (unless (= dd 0) dd) (unless (= h 0) h) (unless (= m 0) m) (unless (= s 0) s))))

(define-timer backup () (:type :single :documentation "Automatically performs configuration backups.")
  (backup))

(defun reschedule (interval)
  (stop-time-handler 'backup)
  (schedule-timer 'backup :every interval))

(define-group backup :documentation "Set backup settings or perform backup actions.")

(define-command (backup interval) (&optional interval (metric "d")) (:authorization T :documentation "Change or view the backup interval.")
  (assert (find metric '("w" "d" "h" "m" "s") :test #'string-equal) () "Metric has to be one of (w  d h m s).")
  (when interval
    (reschedule
     (setf (uc:config-tree :interval)
           (cond ((string= metric "w") (* (parse-integer interval) 60 60 24 7))
                 ((string= metric "d") (* (parse-integer interval) 60 60 24))
                 ((string= metric "h") (* (parse-integer interval) 60 60))
                 ((string= metric "m") (* (parse-integer interval) 60))
                 ((string= metric "s") (parse-integer interval))))))
  (respond event "Current backup interval: ~a" (format-time-since (uc:config-tree :interval))))

(define-command (backup last) () (:documentation "Show the date of the last performed backup.")
  (respond event "Last backup was: ~a" (last-backup module)))

(define-command (backup now) () (:authorization T :documentation "Perform a backup now.")
  (respond event "Backing up now. All modules will be restarted in the process.")
  (backup)
  (respond event "Backup done."))

(define-command (backup restore) (&rest datestring) (:authorization T :documentation "Restore an existing backup.")
  (respond event "Restoring now. All modules will be restarted in the process.")
  (restore (if datestring (format NIL "~{~a~^ ~}" datestring) (last-backup)))
  (respond event "Restore complete."))
