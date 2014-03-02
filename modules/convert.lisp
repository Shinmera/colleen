#|
This file is a part of Colleen
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.convert
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.convert)

(defun json-request (url &rest drakma-params)
  (let ((drakma:*text-content-types* (cons '("application" . "json")
                                           (cons '("text" . "json")
                                                 drakma:*text-content-types*))))
    (cl-json:decode-json-from-string (apply #'drakma:http-request url drakma-params))))

(define-module convert () ()
  (:documentation "Allows various conversions between units and other things."))

(define-group convert-to :documentation "Convert between various formats and units.")

;;                                  A    B       A->B                        B->A
(defparameter *metrics* (list (list "mm" "in"    #'(lambda (x) (* x 0.0394)) #'(lambda (x) (/ x 0.03937)))
                              (list "cm" "ft"    #'(lambda (x) (* x 3.2808)) #'(lambda (x) (/ x 3.2808)))
                              (list "m"  "yd"    #'(lambda (x) (* x 1.0936)) #'(lambda (x) (/ x 1.0936)))
                              (list "km" "mile"  #'(lambda (x) (* x 0.6214)) #'(lambda (x) (/ x 0.6214)))
                              (list "mg" "grain" #'(lambda (x) (* x 0.0154)) #'(lambda (x) (/ x 0.0154)))
                              (list "g"  "oz"    #'(lambda (x) (* x 0.0353)) #'(lambda (x) (/ x 0.0353)))
                              (list "kg" "lb"    #'(lambda (x) (* x 2.2046)) #'(lambda (x) (/ x 2.2046)))
                              (list "t"  "ton"   #'(lambda (x) (* x 0.9842)) #'(lambda (x) (/ x 0.9842)))
                              (list "c"  "f"     #'(lambda (x) (+ (* x (/ 9 5)) 32)) #'(lambda (x) (* (- x 32) (/ 5 9))))))

(define-command (convert-to metric) (unit amount) (:documentation "Convert to metric units. (mm, cm, m, km, mg, g, kg, t, c)")
  (let ((amount (parse-number:parse-number amount))
        (conv (find unit *metrics* :test #'(lambda (x y) (string-equal x (second y))))))
    (if conv
        (respond event "~f ~a" (funcall (fourth conv) amount) (first conv))
        (respond event "Unknown unit."))))

(define-command (convert-to imperial) (unit amount) (:documentation "Convert to imperial units. (in, ft, yd, mile, grain, oz, lb, ton, f)")
  (let ((amount (parse-number:parse-number amount))
        (conv (find unit *metrics* :test #'(lambda (x y) (string-equal x (first y))))))
    (if conv
        (respond event "~f ~a" (funcall (third conv) amount) (second conv))
        (respond event "Unknown unit."))))

(define-command (convert-to tiny) (&rest text) (:documentation "Convert to unicode superscript characters.")
  (respond event "~a" (%map-string text
                                   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-=() "
                                   "ᵃᵇᶜᵈᵉᶠᵍʰᶦʲᵏᶫᵐᶰᵒᵖᑫʳˢᵗᵘᵛʷˣʸᶻᴬᴮᶜᴰᴱᶠᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᑫᴿˢᵀᵁⱽᵂˣʸᶻ⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ ")))

(define-command (convert-to fullwidth) (&rest text) (:documentation "Convert to unicode full-width characters.")
  (respond event "~a" (%map-string text
                                   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789*+-=()<>[]{}\\/_:;$!?^~&%°@# "
                                   "ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ０１２３４５６７８９＊＋－＝（）＜＞[]｛｝＼／＿：；＄！？＾~＆％°＠＃ ")))

(defun %map-string (text from to)
  (flet ((mapper (char)
           (let ((pos (position char from)))
             (if pos (elt to pos) char))))
    (map 'string #'mapper (format NIL "~{~a~^ ~}" text))))

(define-command (convert-to usd) (currency &rest amount) (:documentation "Convert a currency into dollar.")
  (let ((data (json-request "http://rate-exchange.appspot.com/currency" :parameters `(("from" . ,(string-upcase currency)) ("to" . "USD")))))
    (if (cdr (assoc :err data))
        (respond event "Failed to convert: Is the currency correct?")
        (let ((amount (parse-number:parse-number (format NIL "~{~a~^ ~}" amount))))
          (respond event "In USD: ~f" (* (cdr (assoc :rate data)) amount))))))

(define-command (convert-to ascii) (&rest text) (:documentation "Convert into hex ascii.")
  (respond event "~{~2,'0x~}" (map 'list #'char-code (format NIL "~{~a~^ ~}" text))))

(define-command (convert-to ascii-binary) (&rest text) (:documentation "Convert into binary ascii.")
  (respond event "~{~8,'0b~}" (map 'list #'char-code (format NIL "~{~a~^ ~}" text))))

(define-command (convert-to crc32) (&rest text) (:documentation "Create a CRC32 hash of the text.")
  (%hash :crc32 text event))
(define-command (convert-to md5) (&rest text) (:documentation "Create an MD5 hash of the text.")
  (%hash :md5 text event))
(define-command (convert-to sha1) (&rest text) (:documentation "Create a SHA1 hash of the text.")
  (%hash :sha1 text event))
(define-command (convert-to sha256) (&rest text) (:documentation "Create a SHA256 hash of the text.")
  (%hash :sha256 text event))
(define-command (convert-to sha512) (&rest text) (:documentation "Create a SHA512 hash of the text.")
  (%hash :sha512 text event))

(defun %hash (digest text event)
  (respond event "~a" (ironclad:byte-array-to-hex-string
                       (ironclad:digest-sequence
                        digest
                        (ironclad:ascii-string-to-byte-array
                         (format NIL "~{~a~^ ~}" text))))))

(define-command (convert-to pbkdf2) (salt &rest text) (:documentation "Create a salted PBKDF2 hash with SHA512 digest and 1000 iterations.")
  (setf text (format NIL "~{~a~^ ~}"))
  (respond event "~a" (ironclad:byte-array-to-hex-string
                       (ironclad:pbkdf2-hash-password (ironclad:ascii-string-to-byte-array text)
                                                      :salt salt :digest 'ironclad:sha512 :iterations 1000))))

(define-command (convert-to rot13) (&rest text) (:documentation "Encrypt with ROT-13.")
  (respond event "~a" (map 'string
                           (lambda (char &aux (code (char-code char)))
                             (if (alpha-char-p char)
                                 (if (> (- code (char-code (if (upper-case-p char)
                                                               #\A #\a))) 12)
                                     (code-char (- code 13))
                                     (code-char (+ code 13)))
                                 char))
                           text)))

(define-command (convert-to url) (&rest text) (:documentation "URL-encode the text.")
  (respond event "~a" (drakma:url-encode text :utf-8)))

(define-command (convert-to bin) (integer) (:documentation "Convert an integer to its binary representation.")
  (respond event "~b" (parse-integer integer :junk-allowed T)))

(define-command (convert-to hex) (integer) (:documentation "Convert an integer to its hexadecimal representation.") 
  (respond event "~x" (parse-integer integer :junk-allowed T)))

(define-command (convert-to oct) (integer) (:documentation "Convert an integer to its octal representation.")
  (respond event "~o" (parse-integer integer :junk-allowed T)))

(define-command (convert-to base-n) (base integer) (:documentation "Convert an integer to a specific base.")
  (respond event "~a" (write-to-string (parse-integer integer :junk-allowed T) :base (parse-integer base))))

(define-command (convert-to ieee-754) (number) (:documentation "Convert to IEEE-754 32-bit floating point standard.")
  (respond event "~8,'0x" (ieee-floats:encode-float32 (parse-number:parse-number number))))

(define-command (convert-to valve-time) (&rest time) (:documentation "Attempt to convert to valve-time.")
  (declare (ignore time))
  (sleep (+ 1 (/ (random 60) 10)))
  (respond event "Failed to convert to valve-time!"))
