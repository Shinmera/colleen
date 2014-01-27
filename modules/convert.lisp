#|
This file is a part of Colleen
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.convert
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.convert)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :ironclad))

(define-module convert () ()
  (:documentation "Allows various conversions between units and other things."))

(define-group convert-to :documentation "Convert between various formats and units.")

(define-command (convert-to metric) (&rest unit) (:documentation "Convert to metric units.")
  )

(define-command (convert-to imperial) (&rest unit) (:documentation "Convert to imperial units.")
  )

(define-command (convert-to tiny) (&rest text) (:documentation "Convert to unicode superscript characters.")
  )

(define-command (convert-to $) (&rest amount) (:documentation "Convert a currency into dollar.")
  )

(define-command (convert-to ascii) (&rest text) (:documentation "Convert into binary ascii.")
  )

(define-command (convert-to md5) (&rest text) (:documentation "Create an MD5 hash of the text.")
  )

(define-command (convert-to sha256) (&rest text) (:documentation "Create a SHA256 hash of the text.")
  )

(define-command (convert-to pbkdf2) (salt &rest text) (:documentation "Create a salted PBKDF2 hash of the text.")
  )

(define-command (convert-to rot13) (&rest text) (:documentation "Encrypt with ROT-13.")
  )

(define-command (convert-to url) (&rest text) (:documentation "URL-encode the text.")
  )

(define-command (convert-to bin) (number) (:documentation "Convert a number to its binary representation (fixed-point).")
  )

(define-command (convert-to hex) (number) (:documentation "Convert a number to its hexadecimal representation (fixed-point).")
  )

(define-command (convert-to oct) (number) (:documentation "Convert a number to its octal representation (fixed-point).")
  )

(define-command (convert-to ieee-754) (number) (:documentation "Convert to IEEE-754 floating point standard.")
  )

(define-command (convert-to valve-time) (&rest time) (:documentation "Attempt to convert to valve-time.")
  (declare (ignore time))
  (sleep (+ 1 (/ (random 60) 10)))
  (respond event "Failed to convert to valve-time!"))
