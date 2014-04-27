#|
This file is a part of Colleen
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.google
  (:use :cl :colleen :events :split-sequence)
  (:shadow :timezone))
(in-package :org.tymoonnext.colleen.mod.google)

(defvar *language-code-map*)

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

(define-command (google translate-from) (source-language &rest text) (:documentation "Translate a given text from a specific language into english.")
  (with-key (translate-key)
    (multiple-value-bind (translation language) (translate (format NIL "~{~a~^ ~}" text) (translate-key module) :from source-language)
      (declare (ignore language))
      (respond event "[~a → en] ~a" source-language translation))))

(define-command (google translate-from-to) (source-language target-language &rest text) (:documentation "Translate a given text from a language into another.")
  (with-key (translate-key)
    (multiple-value-bind (translation language) (translate (format NIL "~{~a~^ ~}" text) (translate-key module) :from source-language :to target-language)
      (declare (ignore language))
      (respond event "[~a → ~a] ~a" source-language target-language translation))))

(defun ensure-known-language (language)
  (let ((short (or (gethash language *language-code-map*) language)))
    (assert (find short (alexandria:hash-table-values *language-code-map*) :test #'string-equal) () "No such language: ~a" language)
    short))

(defun translate (text api-key &key (to "en") from)
  (let ((parameters `(("key" . ,api-key) ("q" . ,text) ("target" . ,(ensure-known-language to)))))
    (when from (push `("source" . ,(ensure-known-language from)) parameters))
    (let ((json (json-request "https://www.googleapis.com/language/translate/v2" parameters)))
      (let ((data (first (cdr (assoc :translations (cdr (assoc :data json)))))))
        (values (cdr (assoc :translated-text data))
                (cdr (assoc :detected-source-language data)))))))


(define-command (google geocode) (&rest address) (:documentation "Look up geocoding information about an address.")
  (dolist (result (geocode (format NIL "~{~a~^ ~}" address)))
    (respond event "Address: ~a | Type: ~{~a~^, ~} | Coordinates: ~f/~f"
             (cdr (assoc :formatted--address result))
             (cdr (assoc :types result))
             (cdr (assoc :lat (cdr (assoc :location (cdr (assoc :geometry result))))))
             (cdr (assoc :lng (cdr (assoc :location (cdr (assoc :geometry result)))))))))

(define-command (google coordinates) (&rest address) (:documentation "Retrieve the latitude and longitude of an address.")
  (multiple-value-bind (lat lng) (coordinates (format NIL "~{~a~^ ~}" address))
    (if lat
        (respond event "Coordinates: ~a lat, ~a lng" lat lng)
        (respond event "Address not found."))))

(defun coordinates (address)
  (let ((result (first (geocode address))))
    (values (cdr (assoc :lat (cdr (assoc :location (cdr (assoc :geometry result))))))
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
(define-command (google timezone) (&rest address) (:documentation "Retrieve timezone data about a geographical location.")
  (multiple-value-bind (latitude longitude) (coordinates (format NIL "~{~a~^ ~}" address))
    (multiple-value-bind (id name dst raw) (timezone latitude longitude)
      (respond event "~a, ~a (UTC~@f, DST~@f)" id name (/ raw 60 60) (/ dst 60 60)))))

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

(define-command (google time) (&rest address) (:documentation "Retrieve the current local time of a geographical location using the timezone api.")
  (setf address (format NIL "~{~a~^ ~}" address))
  (multiple-value-bind (latitude longitude) (coordinates address)
    (multiple-value-bind (id name dst raw) (timezone latitude longitude)
      (let ((local-time:*default-timezone* local-time:+utc-zone+))
        (respond event "Time for ~a: ~a ~a, ~a (UTC~@f, DST~@f)"
                 address (local-time:format-timestring
                          NIL (local-time:adjust-timestamp! (local-time:now) (offset :sec raw))
                          :format '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))
                 id name (/ raw 60 60) (/ dst 60 60))))))

(define-command (google distance) (origin destination &optional (mode "driving") departure-timestamp) (:documentation "Look up google maps distance data.")
  (assert (find mode '("walking" "driving" "bicycling") :test #'string-equal) () "Mode has to be one of walking, driving, bicycling.")
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
                      (progn
                        (respond event "[~a  → ~a] Error: ~a"
                                 origin destination
                                 (cdr (assoc :status element)))
                        (respond event "[~a  → ~a] Est: ~a"
                                 origin destination
                                 (estimated-distance origin destination)))))))))

(defun distance (origin destination &key (mode "driving") (departure-timestamp (get-unix-time)))
  (let ((json (json-request "https://maps.googleapis.com/maps/api/distancematrix/json"
                            `(("sensor" . "false") ("origins" . ,origin) ("destinations" . ,destination)
                              ("mode" . ,mode) ("departure_time" . ,(format NIL "~d" departure-timestamp))))))
    (if (string-equal (cdr (assoc :status json)) "ok")
        (mapcar #'(lambda (a) (cdr (assoc :elements a))) (cdr (assoc :rows json)))
        (error (cdr (assoc :status json))))))

(defun great-circle-distance (lng1 lat1 lng2 lat2)
  (flet ((rad (x) (/ (* x Pi) 180))
         (haversine (x) (expt (sin (/ x 2)) 2)))
    (let ((earth-radius 6371)
          (lat1 (rad lat1)) (lng1 (rad lng1))
          (lat2 (rad lat2)) (lng2 (rad lng2)))
      (* 2
         earth-radius
         (asin (sqrt (+ (haversine (- lat2 lat1))
                        (* (cos lat1) (cos lat2)
                           (haversine (- lng2 lng1))))))))))

(defun transport-time (distance)
  (flet ((format-and-print (divisor transport-method)
           (format NIL "Human terms: ~,3f hours away by ~a, using an average speed of ~d km/h."
                   (/ distance divisor) transport-method divisor)))
    (cond
      ((< distance 0))
      ((< distance 150)
       (format-and-print 50 "car"))
      ((< distance 2500)
       (format-and-print 130 "train"))
      ((< distance 12500)
       (format-and-print 800 "plane"))
      (T
       (format-and-print 18000 "ICBM")))))

(defun estimated-distance (origin destination)
  (multiple-value-bind (from-latitude from-longitude) (coordinates origin)
    (multiple-value-bind (to-latitude to-longitude) (coordinates destination)
      (let ((distance (great-circle-distance from-longitude from-latitude to-longitude to-latitude)))
        (format NIL "Distance: ~,2f km, ~a" distance
                (transport-time distance))))))
