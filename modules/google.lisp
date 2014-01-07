#|
This file is a part of Colleen
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.google
  (:use :cl :colleen :events :split-sequence))
(in-package :org.tymoonnext.colleen.mod.google)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :parse-number))

(define-module google ()
  ((%translate-key :initform NIL :accessor translate-key))
  (:documentation "Interact with various Google APIs."))

(define-group google :documentation "Interact with various google services.")

(defmethod start ((google google))
  (loop for (key . val) in (config-tree :google :keys)
     do (setf (slot-value google (find-symbol (format NIL "%~a-KEY" key) :org.tymoonnext.colleen.mod.google)) val)))

(defmacro with-key ((slot-accessor &key (eventvar 'event) (modulevar 'module)) &body body)
  `(if (,slot-accessor ,modulevar)
       (progn ,@body)
       (respond ,eventvar "Required API key for this function is not set! Please contact the bot administrator.")))

(defun json-request (url &optional parameters)
  (let* ((drakma:*text-content-types* (cons '("application" . "json")
                                            (cons '("text" . "json")
                                                  drakma:*text-content-types*)))
         (data (drakma:http-request url :parameters parameters :external-format-in :utf-8 :external-format-out :utf-8)))
    (cl-json:decode-json-from-string data)))



(define-command (google translate) (&rest text) (:documentation "Translate a given text into english.")
  (with-key (translate-key)
    (multiple-value-bind (translation language) (translate (format NIL "~{~a~^ ~}" text) (translate-key module))
      (respond event "[~a → en] ~a" language translation))))

(define-command (google translate-to) (target-language &rest text) (:documentation "Translate a given text into a specific language.")
  (with-key (translate-key)
    (multiple-value-bind (translation language) (translate (format NIL "~{~a~^ ~}" text) (translate-key module) :to target-language)
      (respond event "[~a → ~a] ~a" language target-language translation))))

(defun translate (text api-key &key (to "en") from)
  (let ((parameters `(("key" . ,api-key) ("q" . ,text) ("target" . ,to))))
    (when from (push `("source" . ,from) parameters))
    (let ((json (json-request "https://www.googleapis.com/language/translate/v2" parameters)))
      (let ((data (first (cdr (assoc :translations (cdr (assoc :data json)))))))
        (values (cdr (assoc :translated-text data))
                (cdr (assoc :detected-source-language data)))))))


(define-command (google geocode) (&rest address) (:documentation "Look up geocoding information about an address.")
  (dolist (result (geocode (format NIL "~{~a~^ ~}" address)))
    (respond event "Address: ~a | Type: ~{~a~^, ~} | Coordinates: ~f/~f"
             (cdr (assoc :types result))
             (cdr (assoc :formatted--address result))
             (cdr (assoc :lat (cdr (assoc :location (cdr (assoc :geometry result))))))
             (cdr (assoc :lng (cdr (assoc :location (cdr (assoc :geometry result)))))))))

(defun geocode (address)
  (let ((json (json-request "http://maps.googleapis.com/maps/api/geocode/json"
                            `(("address" . ,address) ("sensor" . "false")))))
    (if (string-equal (cdr (assoc :status json)) "ok")
        (cdr (assoc :results json))
        (error (cdr (assoc :status json))))))

(defvar *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0))
(defun get-unix-time ()
  (- (get-universal-time) *unix-epoch-difference*))
(define-command (google timezone) (latitude longitude &optional timestamp) (:documentation "Retrieve timezone data about a geographical location.")
  (let ((latitude (parse-number:parse-number latitude))
        (longitude (parse-number:parse-number longitude))
        (timestamp (if timestamp (parse-integer timestamp) (get-unix-time))))
    (multiple-value-bind (id name dst raw) (timezone latitude longitude timestamp)
      (respond event "~a, ~a (dst: ~d, raw: ~d)" id name dst raw))))

(defun timezone (latitude longitude &optional (timestamp (get-unix-time)))
  (let ((json (json-request "https://maps.googleapis.com/maps/api/timezone/json"
                            `(("sensor" . "false") ("timestamp" . ,(format NIL "~d" timestamp))
                              ("location" . ,(format NIL "~f,~f" latitude longitude))))))
    (if (string-equal (cdr (assoc :status json)) "ok")
        (values (cdr (assoc :time-zone-id json))
                (cdr (assoc :time-zone-name json))
                (cdr (assoc :dst-offset json))
                (cdr (assoc :raw-offset json)))
        (error (cdr (assoc :status json))))))


(define-command (google distance) (origin destination &optional (mode "driving") departure-timestamp) (:documentation "Look up google maps distance data.")
  (assert (find mode '("walking" "driving" "bicycling") :test #'string-equal) () "Mode has to be one of walking, driving, bycicling.")
  (let ((departure-timestamp (if departure-timestamp (parse-integer departure-timestamp) (get-unix-time))))
    (let ((data (distance origin destination :mode mode :departure-timestamp departure-timestamp)))
      (loop for row in data
         for origin in (split-sequence #\| origin)
         for destination in (split-sequence #\| destination)
         do (loop for element in row
               for i from 0
               do (if (string-equal (cdr (assoc :status element)) "ok")
                      (respond event "[~a  → ~a][#~d] Duration: ~a Distance: ~a"
                               origin destination i
                               (cdr (assoc :text (cdr (assoc :duration element))))
                               (cdr (assoc :text (cdr (assoc :distance element)))))
                      (respond event "[~a  → ~a] Error: ~a"
                               origin destination
                               (cdr (assoc :status element)))))))))

(defun distance (origin destination &key (mode "driving") (departure-timestamp (get-unix-time)))
  (let ((json (json-request "https://maps.googleapis.com/maps/api/distancematrix/json"
                            `(("sensor" . "false") ("origins" . ,origin) ("destinations" . ,destination)
                              ("mode" . ,mode) ("departure_time" . ,(format NIL "~d" departure-timestamp))))))
    (v:info :bla "~s" json)
    (if (string-equal (cdr (assoc :status json)) "ok")
        (mapcar #'(lambda (a) (cdr (assoc :elements a))) (cdr (assoc :rows json)))
        (error (cdr (assoc :status json))))))

