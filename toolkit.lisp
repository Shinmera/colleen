#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

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
  (or (config-tree :messages msgsymbol)
      (when otherwise-format (apply #'format NIL otherwise-format))))

(defun fstd-message (event msgsymbol &rest otherwise-format)
  "Shorthand for (format-message (standard-message ..))."
  (let ((msg (or (config-tree :messages msgsymbol)
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

(defmacro with-repeating-restart ((restart-name format-string &rest format-arguments) &body forms)
  `(loop until (with-simple-restart (,restart-name ,format-string ,@format-arguments)
                 ,@forms)))
