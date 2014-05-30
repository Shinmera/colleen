#|
 This file is a part of Colleen
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defvar *timer-map* (make-hash-table)
  "Hash table mapping timer names to time-handler instances.")

(defclass time-handler ()
  ((%identifier :initarg :identifier :initform (error "Identifier required.") :accessor identifier)
   (%timer-type :initarg :timer-type :initform :multiple :accessor timer-type)
   (%arguments :initarg :arguments :initform () :accessor arguments)
   (%handler-function :initarg :handler-function :initform (error "Handler function required.") :accessor handler-function)
   (%launcher-function :initarg :launcher-function :initform #'simple-launcher :accessor launcher-function)
   (%schedulings :initarg :schedulings :initform () :accessor schedulings)
   (%handler-lock :initarg :handler-lock :initform (make-lock) :accessor handler-lock)
   (%docstring :initarg :docstring :initform NIL :accessor docstring))
  (:documentation "Container class representing a time handler."))

(defmethod print-object ((handler time-handler) stream)
  (print-unreadable-object (handler stream :type T)
    (format stream "~s ~s" (identifier handler) (timer-type handler)))
  handler)

(defun time-handler (identifier)
  "Returns the TIME-HANDLER instance associated with the given IDENTIFIER."
  (if (typep identifier 'time-handler)
      identifier
      (gethash identifier *timer-map*)))

(defgeneric (setf time-handler) (time-handler identifier)
  (:documentation "Set a new TIME-HANDLER for the given IDENTIFIER.")
  (:method ((time-handler time-handler) (identifier symbol))
    (assert (eq identifier (identifier time-handler)) ()
            "The identifier of the TIME-HANDLER does not match (~a != ~a)" identifier (identifier time-handler))
    (setf (gethash identifier *timer-map*) time-handler)))

(defun stop-time-handler (identifier)
  "Stops all schedulings for the TIME-HANDLER of the given IDENTIFIER."
  (let ((handler (time-handler identifier)))
    (v:warn :timer "Stopping ~d active timers of ~a." (length (schedulings handler)) identifier)
    (loop for thread in (schedulings handler)
          when (thread-alive-p thread)
            do (destroy-thread thread))))

(defun remove-time-handler (identifier)
  "Removes (and stops) the TIME-HANDLER of the given IDENTIFIER."
  (let ((handler (time-handler identifier)))
    (when handler
      (unless (= 0 (length (schedulings handler)))
        (v:warn :timer "Stopping ~d active timers of ~a due to removal." (length (schedulings handler)) identifier)
        (loop for thread in (schedulings handler)
              do (destroy-thread thread)))
      (remhash identifier *timer-map*))))

(defun set-timer-function (identifier handler-function &key (type :multiple) (arguments () a-p) (launcher #'simple-launcher) docstring)
  "Set a new timer function.

IDENTIFIER       --- A symbol identifying the handler.
HANDLER-FUNCTION --- The function to run once a scheduling triggers. Has
                     to have the same arguments lambda-list as specified
                     by ARGUMENTS.
TYPE             --- Either :SINGLE or :MULTIPLE, which declares whether
                     the timer should be scheduled only once or can be
                     scheduled many times. Note that this does not enforce
                     the behaviour, only signals it to the user.
ARGUMENTS        --- A lambda-list for the arguments the HANDLER-FUNCTION
                     expects. If not provided, it will attempt to retrieve
                     it through FUNCTION-ARGUMENTS.
LAUNCHER         --- A function that expects two arguments, the scheduluing
                     thread it itself is called in and a function it should
                     call. Changing this is only useful if you need to
                     establish an environment around the timer call itself.
DOCSTRING        --- An optional documentation string."
  (assert (symbolp identifier) () "IDENTIFIER has to be a symbol.")
  (let ((handler (time-handler identifier)))
    (when handler
      (v:debug :timer "Redefining handler ~a" identifier)
      (when (schedulings handler)
        (v:warn :timer "Redefining running timer ~a" identifier)
        (loop for thread in (schedulings handler)
              do (destroy-thread thread)))))
  (unless a-p
    (setf arguments (cdr (function-arguments handler-function))))
  (setf (time-handler identifier)
        (make-instance 'time-handler 
                       :identifier identifier
                       :handler-function handler-function :launcher-function launcher
                       :timer-type type :docstring docstring)))

(defgeneric apropos-time-handler (handler)
  (:documentation "Returns a string describing the given time-handler if it exists.")
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
  "Schedule a new scheduling for the TIMER.

TIMER     --- A symbol naming the time-handler.
TYPE      --- Can be one of :DATE :TIME :ONCE :WHEN, in which case
              the scheduling only occurs once, or it can be one of
              :INTERVAL :REPEATING :LOOP :EVERY, in which case it is
              run repeatedly.
DELAY     --- A time designator as parsed by PARSE-DELAY.
ARGUMENTS --- The arguments list to call the timer with.
IF-EXISTS --- What to do if a scheduling already exists for the timer.
              :ADD adds a new scheduling, :ERROR signals a condition,
              NIL does nothing. This defaults to :ERROR on :SINGLE
              timers and to :ADD on :MULTIPLE timers."
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

(defun reschedule-timer (timer type delay &key arguments (replace :all))
  "Reschedule the given TIMER by replacing an existing scheduling or all.

TIMER     --- A symbol naming the time-handler.
TYPE      --- Can be one of :DATE :TIME :ONCE :WHEN, in which case
              the scheduling only occurs once, or it can be one of
              :INTERVAL :REPEATING :LOOP :EVERY, in which case it is
              run repeatedly.
DELAY     --- A time designator as parsed by PARSE-DELAY.
ARGUMENTS --- The arguments list to call the timer with.
REPLACE   --- Can be :ALL, :FIRST, :LAST or an integer of the position
              to replace."
  (assert (find type '(:date :time :once :when :interval :repeating :loop :every)) ()
          "TYPE must be one of (:DATE :TIME :ONCE :INTERVAL :REPEATING :LOOP).")
  (assert (or (find replace '(:all :first :last))
              (typep replace 'integer)) ()
          "IF-EXISTS must be one of (:ERROR :ADD NIL INTEGER)")
  (let ((handler (time-handler timer))
        (delay (parse-delay delay)))
    (unless handler
      (error "No such time-handler found: ~a" timer))
    (v:debug :timer "Rescheduling timer ~a (replacing ~a) with ~a ~a" timer replace type delay)
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
          (let ((new-thread (pop (schedulings handler)))
                (old-thread (nth position (schedulings handler))))
            (when (thread-alive-p old-thread)
              (destroy-thread old-thread))
            (setf (nth position (schedulings handler)) new-thread)))))
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
  "Defines a new timer with the given NAME.

NAME          --- A symbol identifying your timer.
ARGS          --- A list of arguments the timer expects. This is a
                  reduced lambda-list that can only accept required,
                  optional and rest args. See SET-TIMER-FUNCTION.
TYPE          --- Either :SINGLE or :MULTIPLE. See SET-TIMER-FUNCTION.
DOCUMENTATION --- An optional docstring.
EVENTVAR      --- Symbol to bind the event variable to.
MODULE-NAME   --- An optional name to activate module convenience bindings.
                  Defaults to GET-CURRENT-MODULE-NAME when unset.
MODULEVAR     --- The symbol to bind the module instance to, if at all.
BODY          ::= form*"
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
