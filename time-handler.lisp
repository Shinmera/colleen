#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defvar *timer-map* (make-hash-table)
  "")

(defclass time-handler ()
  ((%identifier :initarg :identifier :initform (error "Identifier required.") :accessor identifier)
   (%launcher-function :initarg :launcher-function :initform (error "Launcher function required.") :accessor launcher-function)
   (%handler-function :initarg :handler-function :initform (error "Handler function required.") :accessor handler-function)
   (%active-timers :initarg :active-timers :initform () :accessor active-timers)
   (%handler-lock :initarg :handler-lock :initform (make-lock) :accessor handler-lock)
   (%docstring :initarg :docstring :initform NIL :accessor docstring))
  (:documentation ""))

(defmethod print-object ((handler time-handler) stream)
  (print-unreadable-object (handler stream :type T)
    (format stream "~s" (identifier handler)))
  handler)

(defun time-handler (identifier)
  ""
  (if (typep identifier 'time-handler)
      identifier
      (gethash identifier *timer-map*)))

(defgeneric (setf time-handler) (time-handler identifier)
  (:documentation "")
  (:method ((time-handler time-handler) (identifier symbol))
    (assert (eq identifier (identifier time-handler)) ()
            "The identifier of the TIME-HANDLER does not match (~a != ~a)" identifier (identifier time-handler))
    (setf (gethash identifier *timer-map*) time-handler)))

(defun stop-time-handler (identifier)
  ""
  (let ((handler (time-handler identifier)))
    (v:warn :timer "Stopping ~d active timers of ~a." (length (active-timers handler)) identifier)
    (loop for thread in (active-timers handler)
          when (thread-alive-p thread)
            do (destroy-thread thread))))

(defun remove-time-handler (identifier)
  ""
  (let ((handler (time-handler identifier)))
    (when handler
      (unless (= 0 (length (active-timers handler)))
        (v:warn :timer "Stopping ~d active timers of ~a due to removal." (length (active-timers handler)) identifier)
        (loop for thread in (active-timers handler)
              do (destroy-thread thread)))
      (remhash identifier *timer-map*))))

(defun set-timer-function (identifier launcher-function handler-function &key docstring)
  ""
  (assert (symbolp identifier) () "IDENTIFIER has to be a symbol.")
  (when (time-handler identifier)
    (v:debug :timer "Redefining handler ~a" identifier)
    (when (active-timers (time-handler identifier))
      (v:warn :timer "Redefining running timer ~a" identifier)))
  (setf (time-handler identifier)
        (make-instance 'time-handler
                       :identifier identifier :launcher-function launcher-function
                       :handler-function handler-function :docstring docstring)))

(defgeneric apropos-time-handler (handler)
  (:documentation "")
  (:method ((identifier symbol))
    (apropos-time-handler (time-handler identifier)))
  (:method ((handler time-handler))
    (let ((*print-pretty* NIL))
      (format NIL "[Time Handler] ~s with ~d active timers.~%~
                 ~:[No docstring available.~;Docstring: ~:*~a~]"
              (identifier handler) (length (active-timers handler)) (docstring handler)))))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))
(defgeneric parse-delay (delay)
  (:documentation "Parses a delay to be used in timers.

If the delay is an INTEGER, it is returned as-is.
If the delay is a LIST, the following keywords may be used:

  :SEC :MIN :HOUR :DAY :MONTH :YEAR
    Creates an absolute (universal-time) timestamp for the
    specified date. If a parameter is not specified, the
    current time component (current day, hour, etc) is used.

  :UNIX :UNIVERSAL
    Creates an absolute (universal-time) timestamp for the
    specified date.

  :SECS :MINS :HOURS :DAYS :MONTHS :YEARS
    Creates a relative (in seconds) timestamp for the
    specified timespan.

  :FROM :EVERY
    Creates a timestamp with an initial delay specified by
    :FROM and a following delay of :EVERY. Both parameters
    are again parsed with PARSE-DELAY.")
  (:method ((delay integer))
    delay)
  (:method ((delay cons))
    (destructuring-bind (&key sec min hour day month year secs mins hours days months years unix universal from every) delay
      (unless (xor (or sec min hour day month year)
                   (or secs mins hours days months years)
                   (or unix universal)
                   (or from every))
        (error "Cannot use multiple delay descriptors at the same time."))
      (cond
        ((or sec min hour day month year)
         (let ((now (local-time:now)))
           (local-time:timestamp-to-universal
            (local-time:encode-timestamp
             0
             (or sec (local-time:timestamp-second now))
             (or min (local-time:timestamp-minute now))
             (or hour (local-time:timestamp-hour now))
             (or day (local-time:timestamp-day now))
             (or month (local-time:timestamp-month now))
             (or year (local-time:timestamp-year now))))))
        ((or secs mins hours days months years)
         (+ (* (or secs 0) 1)
            (* (or mins 0) 60)
            (* (or hours 0) 60 60)
            (* (or days 0) 60 60 24)
            (* (or months 0) 60 60 24 30)
            (* (or years 0) 60 60 24 365)))
        ((or unix universal)
         (if universal
             universal
             (+ unix *unix-epoch-difference*)))
        ((and from every)
         (cons (parse-delay from)
               (parse-delay every)))))))

(defun schedule-timer (timer type delay)
  ""
  (assert (find type '(:date :time :once :when :interval :repeating :loop :every)) ()
          "TYPE must be one of (:DATE :TIME :ONCE :INTERVAL :REPEATING :LOOP).")
  (let ((handler (time-handler timer))
        (delay (parse-delay delay)))
    (unless handler
      (error "No such time-handler found: ~a" timer))
    (v:debug :timer "Scheduling timer ~a with ~a ~a" timer type delay)
    (funcall (launcher-function handler)
             type delay)))

(defun timer-timeout (type delay function)
  (flet ((inner (time-target)
           (v:trace :timer "Timing ~a seconds" (- time-target (get-universal-time)))
           (loop while (< (get-universal-time) time-target)
                 do (sleep (- time-target (get-universal-time))))
           (funcall function)))
    (ecase type
      ((:date :time :once :when)
       (inner (if (consp delay) (car delay) delay)))
      ((:interval :repeating :loop :every)
       (if (consp delay)
           (loop for current = (car delay)
                   then (+ (get-universal-time) (cdr delay))
                 do (inner current)
                    (thread-yield))
           (loop do (inner (+ (get-universal-time) delay))
                    (thread-yield)))))))

(defun launch-module-timer (handler-name module-name type delay)
  (let ((handler (time-handler handler-name))
        (module (get-module module-name))
        (uuid) (launch-done NIL))
    (setf uuid (with-module-thread (module)
                 (unwind-protect
                      (timer-timeout type delay (handler-function handler))
                   ;; Yield until the launch has completed.
                   (loop until launch-done do (thread-yield))
                   (with-lock-held ((handler-lock handler))
                     (let ((thread (module-thread module uuid)))
                       (v:debug :timer "CLEANUP ~a" thread)
                       (setf (active-timers handler)
                             (delete thread (active-timers handler))))))))
    (let ((thread (module-thread module uuid)))
      (with-lock-held ((handler-lock handler))
        (push thread (active-timers handler)))
      ;; We can only set this now to ensure the thread definitely waits
      ;; with its self-removal until we've actually pushed it.
      (setf launch-done T)
      thread)))

(defun launch-generic-timer (handler-name type delay)
  (let ((handler (time-handler handler-name))
        (thread NIL) (launch-done NIL))
    (setf thread (make-thread #'(lambda ()
                                  (unwind-protect
                                       (timer-timeout type delay (handler-function handler))
                                    ;; Yield until the launch has completed.
                                    (loop until launch-done do (thread-yield))
                                    (with-lock-held ((handler-lock handler))
                                      (v:debug :timer "CLEANUP ~a" thread)
                                      (setf (active-timers handler)
                                            (delete thread (active-timers handler))))))
                              :name (format NIL "Timer thread for ~a" handler-name)))
    (with-lock-held ((handler-lock handler))
      (push thread (active-timers handler)))
    ;; We can only set this now to ensure the thread definitely waits
    ;; with its self-removal until we've actually pushed it.
    (setf launch-done T)
    thread))

(defmacro define-timer (name (&key documentation module-name (modulevar 'module)) &body body)
  ""
  (unless module-name
    (setf module-name (get-current-module-name)))
  (let ((launcher (gensym "LAUNCHER-FUNCTION"))
        (handler (gensym "HANDLER-FUNCTION")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,handler
               #'(lambda ()
                   ,@(if module-name
                         `((with-module (,modulevar ,module-name)
                             (when (active ,modulevar)
                               (with-module-lock (,modulevar)
                                 (with-module-storage (,modulevar)
                                   ,@body)))))
                         body)))
             (,launcher
               #'(lambda (type delay)
                   ,(if module-name
                        `(launch-module-timer ',name ',module-name type delay)
                        `(launch-generic-timer ',name type delay)))))
         (set-timer-function ',name ,launcher ,handler :docstring ,documentation)))))
