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
   (%timer-type :initarg :timer-type :initform :multiple :accessor timer-type)
   (%arguments :initarg :arguments :initform () :accessor arguments)
   (%handler-function :initarg :handler-function :initform (error "Handler function required.") :accessor handler-function)
   (%launcher-function :initarg :launcher-function :initform #'simple-launcher :accessor launcher-function)
   (%schedulings :initarg :schedulings :initform () :accessor schedulings)
   (%handler-lock :initarg :handler-lock :initform (make-lock) :accessor handler-lock)
   (%docstring :initarg :docstring :initform NIL :accessor docstring))
  (:documentation ""))

(defmethod print-object ((handler time-handler) stream)
  (print-unreadable-object (handler stream :type T)
    (format stream "~s ~s" (identifier handler) (timer-type handler)))
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
    (v:warn :timer "Stopping ~d active timers of ~a." (length (schedulings handler)) identifier)
    (loop for thread in (schedulings handler)
          when (thread-alive-p thread)
            do (destroy-thread thread))))

(defun remove-time-handler (identifier)
  ""
  (let ((handler (time-handler identifier)))
    (when handler
      (unless (= 0 (length (schedulings handler)))
        (v:warn :timer "Stopping ~d active timers of ~a due to removal." (length (schedulings handler)) identifier)
        (loop for thread in (schedulings handler)
              do (destroy-thread thread)))
      (remhash identifier *timer-map*))))

(defun set-timer-function (identifier handler-function &key (type :multiple) (arguments () a-p) (launcher #'simple-launcher) docstring)
  ""
  (assert (symbolp identifier) () "IDENTIFIER has to be a symbol.")
  (when (time-handler identifier)
    (v:debug :timer "Redefining handler ~a" identifier)
    (when (schedulings (time-handler identifier))
      (v:warn :timer "Redefining running timer ~a" identifier)))
  (unless a-p
    (setf arguments (cdr (function-arguments handler-function))))
  (setf (time-handler identifier)
        (make-instance 'time-handler 
                       :identifier identifier
                       :handler-function handler-function :launcher-function launcher
                       :timer-type type :docstring docstring)))

(defgeneric apropos-time-handler (handler)
  (:documentation "")
  (:method ((identifier symbol))
    (apropos-time-handler (time-handler identifier)))
  (:method ((handler time-handler))
    (let ((*print-pretty* NIL))
      (format NIL "[Time Handler] ~s with ~d active timers expecting ~a.~%~
                 ~:[No docstring available.~;Docstring: ~:*~a~]"
              (identifier handler) (length (schedulings handler)) (arguments handler) (docstring handler)))))

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

  :IN
    Creates an absolute (universal-time) timestamp using
    the specified relative sub-delay, which is parsed by
    PARSE-DELAY.

  :FROM :EVERY :UNTIL
    Creates a timestamp with an initial delay specified by
    :FROM and a following delay of :EVERY. Both parameters
    are again parsed with PARSE-DELAY. If :UNTIL is passed,
    the timestamp will only repeat until the absolute 
    timestamp :UNTIL is reached.")
  (:method ((delay integer))
    delay)
  (:method ((delay cons))
    (destructuring-bind (&key sec min hour day month year secs mins hours days months years unix universal in from every until) delay
      (unless (xor (or sec min hour day month year)
                   (or secs mins hours days months years)
                   (or unix universal)
                   (or in)
                   (or from every until))
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
        ((or in)
         (+ (get-universal-time)
            (parse-delay in)))
        ((and from every)
         (list (parse-delay from)
               (parse-delay every)
               (if until (parse-delay until) -1)))))))

(defun timer-timeout (type delay handler arguments)
  (flet ((inner (time-target)
           (v:trace :timer "Timing ~a seconds" (- time-target (get-universal-time)))
           (loop while (< (get-universal-time) time-target)
                 do (sleep (- time-target (get-universal-time))))
           (handler-bind
               ((error #'(lambda (err)
                           (v:warn :timer "ERROR: ~a" err)
                           (when *debugger*
                             (invoke-debugger err)))))
             (with-simple-restart (skip-timer-call "Skip calling the timer. May try again if it is a looping schedule.")
               (loop until (with-simple-restart (retry-timer-call "Retry calling the timer.")
                             (apply (handler-function handler) arguments) T))))))
    (ecase type
      ((:date :time :once :when)
       (inner (if (consp delay) (car delay) delay)))
      ((:interval :repeating :loop :every)
       (if (listp delay)
           (destructuring-bind (from every until) delay
             (loop for current = from
                     then (+ (get-universal-time) every)
                   while (or (< until 0)
                             (< (get-universal-time) until))
                   do (inner current)
                      (thread-yield)))
           (loop do (inner (+ (get-universal-time) delay))
                    (thread-yield)))))))

(defun launch-timer (handler type delay arguments)
  (let ((thread) (launch-done NIL))
    (setf thread (make-thread #'(lambda ()
                                  ;; Yield until the launch has completed.
                                  (loop do (thread-yield) until launch-done)                                  
                                  (unwind-protect
                                       (with-simple-restart (skip-launcher "Skip the timer launcher entirely, essentially ending the scheduling.")
                                         (funcall (launcher-function handler)
                                                  thread #'(lambda () (timer-timeout type delay handler arguments))))
                                    (with-lock-held ((handler-lock handler))
                                      (v:trace :timer "Cleaning up ~a" thread)
                                      (setf (schedulings handler)
                                            (delete thread (schedulings handler))))))
                              :name (format NIL "Timer thread for ~a" handler)))
    (with-lock-held ((handler-lock handler))
      (push thread (schedulings handler)))
    ;; We can only set this now to ensure the thread definitely waits
    ;; with its self-removal until we've actually pushed it.
    (setf launch-done T)
    thread))

(defun schedule-timer (timer type delay &key arguments (if-exists NIL iep))
  ""
  (assert (member type '(:date :time :once :when :interval :repeating :loop :every)) ()
          "TYPE must be one of (:DATE :TIME :ONCE :INTERVAL :REPEATING :LOOP).")
  (assert (member if-exists '(:error :add NIL)) ()
          "IF-EXISTS must be one of (:ERROR :ADD NIL) ~a")
  (let ((handler (time-handler timer))
        (delay (parse-delay delay)))
    (unless handler
      (error "No such time-handler found: ~a" timer))
    (unless iep
      (case (timer-type handler)
        (:multiple (setf if-exists :add))
        (:single (setf if-exists :error))))
    (when (schedulings handler)
      (with-simple-restart (add-scheduling "Add a scheduling anyway.")
        (case if-exists
          (:add)
          (:error (error "A scheduling is already running on this timer!"))
          (T (return-from schedule-timer NIL)))))
    (v:debug :timer "Scheduling timer ~a with ~a ~a" timer type delay)
    (launch-timer handler type delay arguments))
  T)

(defun reschedule-timer (identifier type delay &key arguments (replace :all))
  ""
  (assert (find type '(:date :time :once :when :interval :repeating :loop :every)) ()
          "TYPE must be one of (:DATE :TIME :ONCE :INTERVAL :REPEATING :LOOP).")
  (assert (or (find replace '(:all :first :last))
              (typep replace 'integer)) ()
          "IF-EXISTS must be one of (:ERROR :ADD NIL INTEGER)")
  (let ((handler (time-handler identifier))
        (delay (parse-delay delay)))
    (unless handler
      (error "No such time-handler found: ~a" identifier))
    (v:debug :timer "Rescheduling timer ~a (replacing ~a) with ~a ~a" identifier replace type delay)
    (if (eq replace :all)
        (loop for thread = (pop (schedulings handler))
              while thread do (when (thread-alive-p thread)
                                (destroy-thread thread))
              finally (launch-timer handler type delay arguments))
        (let ((position (case replace
                          (:first 0)
                          (:last (1- (length (schedulings handler))))
                          (T replace))))
          (launch-timer handler type delay arguments)
          (setf (nth position (schedulings handler))
                (pop (schedulings handler))))))
  T)

(defun simple-launcher (thread function)
  (declare (ignore thread))
  (funcall function))

(defun module-launcher (module-name thread function)
  (let ((uuid (princ-to-string (uuid:make-v4-uuid)))
        (module (get-module module-name)))
    (setf (module-thread module uuid) thread)
    (unwind-protect
         (handler-case
             (handler-bind
                 ((error #'(lambda (err)
                             (v:severe module-name "Unexpected error at thread-level: ~a" err)
                             (when *debugger*
                               (invoke-debugger err)))))
               (funcall function))
           (module-stop (err)
             (declare (ignore err))
             (v:debug module-name "Received module-stop condition, leaving thread ~a." uuid)))
      (v:trace module-name "Ending thread ~a." uuid)
      (remhash uuid (threads module)))))

(defmacro define-timer (name (&rest args) (&key (type :multiple) documentation module-name (modulevar 'module)) &body body)
  ""
  (assert (find type '(:MULTIPLE :SINGLE)) () "TYPE must be one of (:MULTIPLE :SINGLE)")
  (unless module-name
    (setf module-name (get-current-module-name)))
  (let ((handler (gensym "HANDLER-FUNCTION")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,handler
               #'(lambda ,args
                   ,@(if module-name
                         `((with-module (,modulevar ,module-name)
                             (when (active ,modulevar)
                               (with-module-lock (,modulevar)
                                 (with-module-storage (,modulevar)
                                   ,@body)))))
                         body))))
         (set-timer-function ',name ,handler :type ,type :docstring ,documentation
                             ,@(when module-name
                                 `(:launcher #'(lambda (thread function)
                                                 (module-launcher ,module-name thread function)))))))))
