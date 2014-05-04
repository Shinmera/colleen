#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.weather
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.weather)

(define-module weather () ()
  (:documentation "Check the weather status on locations."))

(defvar *location-api* "https://maps.googleapis.com/maps/api/geocode/json")
(defvar *weather-api* "https://api.forecast.io/forecast/~a/~a,~a,~a?units=si&exclude=hourly,daily,flags")

(defun get-coordinates (location)
  (let* ((stream (drakma:http-request *location-api* :parameters `(("sensor" . "false") ("address" . ,location))
                                      :want-stream T :external-format-in :utf-8 :external-format-out :utf-8))
         (result (json:decode-json stream))
         (data (cdr (assoc :location 
               (cdr (assoc :geometry 
               (first (cdr (assoc :results result)))))))))
    (close stream)
    (values
     (list (cdr (assoc :lat data))
           (cdr (assoc :lng data)))
     (cdr (assoc :long--name (first (cdr (assoc :address--components (first (cdr (assoc :results result)))))))))))

(defvar *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0))
(defun get-unix-time ()
  (- (get-universal-time) *unix-epoch-difference*))

(defun get-weather (latitude longitude &optional (timestamp (get-unix-time)) (apikey (gethash :apikey (storage (get-module :weather)))))
  (let* ((stream (drakma:http-request (format NIL *weather-api* apikey latitude longitude timestamp) :want-stream T  :external-format-in :utf-8))
         (data (json:decode-json stream)))
    (close stream)
    (cdr (assoc :currently data))))

(define-command weather (&rest location) (:documentation "Retrieve the current weather data of a location.")
  (setf location (format NIL "~{~a~^ ~}" location))
  (multiple-value-bind (coords location) (get-coordinates location)
    (let ((data (apply #'get-weather coords)))
      (flet ((d (field) (cdr (assoc field data))))
        (if data
            (respond event "Weather for ~a: ~a at ~a°C~:[ (feels like ~a°C)~;~*~], ~a% humidity, ~akm/h wind, ~ahPa pressure."
                     location (d :summary) (d :temperature)
                     (= (d :temperature) (d :apparent-temperature)) (d :apparent-temperature)
                     (round (* 100 (d :humidity))) (d :wind-speed) (d :pressure)) 
            (respond event "Sorry, I couldn't find any data for ~a." location))))))
