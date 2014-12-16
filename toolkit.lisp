#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defun lambda-list->string (lambda-list)
  (with-output-to-string (stream)
    (labels ((p (token)
               (typecase token
                 (null
                  (write-string "()" stream))
                 (list
                  (write-char #\( stream)
                  (p (car token))
                  (loop for item in (cdr token)
                        do (write-char #\Space stream)
                           (p item))
                  (write-char #\) stream))
                 (symbol
                  (write-string (string-downcase (symbol-name token)) stream)))))
      (p lambda-list))))

(defun format-message (event message &rest other-replaces)
  "Format the given message, replacing $NICK$, $CHANNEL$, $SERVER$ and $MYSELF$ with the according values."
  (let ((replaces (append other-replaces
                          `(("\\$NICK\\$" ,(nick event))
                            ("\\$CHANNEL\\$" ,(channel event))
                            ("\\$SERVER\\$" ,(string (name (server event))))
                            ("\\$MYSELF\\$" ,(nick (server event)))))))
    (flet ((rep (search replace message)
             (cl-ppcre:regex-replace-all search message replace)))
      (loop for replace in replaces
            do (setf message (rep (car replace) (cdr replace) message))
            finally (return message)))))

(defun standard-message (msgsymbol &rest otherwise-format)
  "Returns the default message designated by the symbol or if provided the otherwise-formatted string."
  (or (bot-config :messages msgsymbol)
      (when otherwise-format (apply #'format NIL otherwise-format))))

(defun fstd-message (event msgsymbol &rest otherwise-format)
  "Shorthand for (format-message (standard-message ..))."
  (let ((msg (or (bot-config :messages msgsymbol)
                 (when otherwise-format (apply #'format NIL otherwise-format)))))
    (when msg (format-message event msg))))

(defparameter *mirc-color-regex* (cl-ppcre:create-scanner "\\x03([0-9][1-5]?)?(,([0-9][1-5]?))?"))
(defparameter *formatting-regex* (cl-ppcre:create-scanner "[\\x02\\x1F\\x16\\x0F]"))
(defparameter *mirc-color-map* (let ((map (make-hash-table)))
                                 (loop for val in '(:white :black :blue :green :red :brown :purple :orange :yellow :light-green :teal :cyan :light-blue :pink :grey :light-grey)
                                       for key from 0
                                       do (setf (gethash key map) val))
                                 map))

(defun mirc-color->name (num)
  (when (stringp num)
    (setf num (parse-integer num)))
  (gethash num *mirc-color-map* :?))

(defun strip-colors (string)
  "Strips mIRC color codes from the string."
  (cl-ppcre:regex-replace-all *mirc-color-regex* string "") "")

(defun color-entities (string)
  "Returns two values:
  1. the formatting stripped string
  2. a list of mIRC formatting entities found in the string.
Each entity is a list with the following format: (TYPE START END)"
  (let ((entities ())
        (offset 0)
        (unclosed-fg NIL)
        (unclosed-bg NIL)
        (start (length string)))
    (macrolet ((finish (var)
                 `(when ,var
                    (setf (third ,var) (- start offset))
                    (push ,var entities))))
      (cl-ppcre:do-matches (start end *mirc-color-regex* string)
        (cl-ppcre:register-groups-bind (fg NIL bg) (*mirc-color-regex* (subseq string start end))
          (when fg
            (finish unclosed-fg)
            (setf unclosed-fg (list (cons :foreground (mirc-color->name fg)) (- end offset) :?)))
          (when bg
            (finish unclosed-bg)
            (setf unclosed-bg (list (cons :background (mirc-color->name bg)) (- end offset) :?)))
          (when (not (or bg fg))
            (finish unclosed-fg)
            (finish unclosed-bg)
            (setf unclosed-fg NIL unclosed-bg NIL))
          (incf offset (- end start))))
      (finish unclosed-fg)
      (finish unclosed-bg))
    (values (strip-colors string) entities)))

(defun break-string (string &key (limit *irc-message-limit*) (max-backstep 30) (break-char #\Newline) (latch-char #\Space))
  "Break the string into parts of a maximal length of LIMIT, intermitting BREAK-CHAR.
If possible, use LATCH-CHAR to find a more apropriate breaking point within the limit of MAX-BACKSTEP."
  (string-trim
   '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page)
   (with-output-to-string (stream)
     (loop for last = 0 then pos
           for pos = limit then (+ pos limit)
           until (<= (length string) pos)
           do (let ((breakpos (or (position latch-char string :from-end T :end pos :start (if (< pos max-backstep) 0 (- pos max-backstep)) :test #'char=)
                                  pos)))
                (write-string (subseq string last breakpos) stream)
                (write-char break-char stream)
                (setf pos (1+ breakpos)))
           finally (write-string (subseq string last (length string)) stream)))))

(defmacro with-repeating-restart ((restart-name format-string &rest format-arguments) &body forms)
  `(loop for ret = (with-simple-restart (,restart-name ,format-string ,@format-arguments)
                     ,@forms)
         until ret finally (return ret)))

(defun escape-regex-symbols (string)
  (cl-ppcre:regex-replace-all "([\\\\\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}])" string '("\\" 0)))

(defun lambda-keyword-p (symbol)
  (find symbol '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

(defun flatten-lambda-list (lambda-list)
  (mapcar #'(lambda (a) (if (listp a) (car a) a)) lambda-list))

(defun extract-lambda-vars (lambda-list)
  (remove-if #'lambda-keyword-p (flatten-lambda-list lambda-list)))

(defun required-lambda-vars (lambda-list)
  (loop for i in lambda-list
        until (lambda-keyword-p i)
        collect i))

(defun build-lambda-call (lambda-list)
  (loop with return = ()
        with keys = NIL
        for arg in (flatten-lambda-list lambda-list)
        do (if keys
               (setf return (cons arg (cons (find-symbol (symbol-name arg) "KEYWORD") return)))
               (unless (lambda-keyword-p arg)
                 (push arg return)))
           (when (eq arg '&key)
             (setf keys T))
        finally (return (nreverse return))))

(defun force-release-lock (lock)
  "Attempts to forcefully release the lock.
This is only implemented with: SBCL"
  #+sbcl (sb-thread:release-mutex lock :if-not-owner :force))
