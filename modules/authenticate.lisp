#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.auth
  (:use :cl :colleen)
  (:shadowing-import-from :colleen :restart))
(in-package :org.tymoonnext.colleen.mod.auth)

(define-module auth () ())

(define-handler nick-change auth (nick-event event)
  (with-accessors ((prevnick old-nick) (newnick nick)) event
    (when (find prevnick (auth-users (server event)) :test #'string=)
      (v:info (name (server event)) "Changing ~a to ~a in authenticated list due to NICK." prevnick newnick)
      (setf (auth-users (server event))
            (cons newnick (delete prevnick (auth-users (server event)) :test #'string=))))))

(define-handler part-remove auth (part-event event)
  (when (auth-p (nick event))
    (remove-from-auth (nick event) "PART")))

(define-handler quit-remove auth (quit-event event)
  (when (auth-p (nick event))
    (remove-from-auth (nick event) "QUIT")))

(define-handler kick-remove auth (kick-event event)
  (when (auth-p (nick event))
    (remove-from-auth (nick event) "KICK")))

(define-command logout auth () ()
  (if (auth-p (nick event))
      (progn
        (remove-from-auth (nick event) "logout command")
        (respond event (fstd-message event :auth-out)))
      (respond event (fstd-message event :auth-fail))))

(defvar *pending-nickserv-auth* ())
(defvar *nickserv-status-regex* (cl-ppcre:create-scanner "STATUS (.+) ([0-3])"))

(define-command login auth (&rest pw) ()
  (handler-case 
      (if (auth-p (nick event))
          (respond event (fstd-message event :auth-already))
          (let ((logininfo (config-tree :auth :logins (find-symbol (string-upcase (nick event)) "KEYWORD"))))
            (if pw
                (if (string= (format nil "~{~a~^ ~}" pw) logininfo)
                    (progn (add-to-auth (nick event) "Password auth.")
                           (respond event (fstd-message event :auth)))
                    (progn (v:warn :auth "User (~a)~a tried to authenticate with ~a but failed." (name (server event)) (nick event) pw)
                           (respond event (fstd-message event :auth-fail))))
                (if logininfo
                    (progn
                      (v:debug :auth "Requesting NickServ status for ~a" (nick event))
                      (irc:privmsg "NickServ" (format nil "STATUS ~a" (nick event)))
                      (push event *pending-nickserv-auth*)
                      (respond event (fstd-message event :auth-wait)))
                    (progn
                      (v:warn :auth "User ~a tried to authenticate through NickServ, but isn't on ident list!" (nick event))
                      (respond event (fstd-message event :auth-fail)))))))
    (error (err)
      (v:warn :auth "Error during login attempt: ~a" err)
      (respond event (fstd-message event :auth-fail)))))

(define-handler nickserv-login auth (notice-event notice-event)
  (when (string-equal "NickServ" (nick notice-event))
    (cl-ppcre:register-groups-bind (user level) (*nickserv-status-regex* (message notice-event))
      (let ((event (loop for pending in *pending-nickserv-auth*
                      do (if (string-equal user (nick pending)) (return pending)))))
        (if event
            (if (string= level "3")
                (progn
                  (add-to-auth user "NickServ IDENT.")
                  (setf *pending-nickserv-auth*
                        (delete event *pending-nickserv-auth*))
                  (respond event (fstd-message event :auth)))
                (progn
                  (v:warn :auth "Received bad NickServ STATUS for user ~a: ~d" user level)
                  (respond event (fstd-message event :auth-fail))))
            (v:warn :auth "Received NickServ STATUS for user (~a)~a without pending request!" (name (server notice-event)) user))))))

