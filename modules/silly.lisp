#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.silly
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.silly)

(define-module silly () ()
  (:documentation "Silly things."))

(define-handler (privmsg-event event) ()
  (when (cl-ppcre:scan "^colleen: (and )?(oh )?i ((love)|(luv)|(wub)) ((you)|(u)|(wu)|(wuu)|(wo)|(woo))( too)?( as well)?( ((so)|(very)|(too))( much)?)?$" (string-downcase (message event)))
    (respond event (format NIL "~a: ~a ~a" 
                           (nick event) 
                           (alexandria:random-elt '("I love you too." "Aww!" "Oh you~~" "Haha, oh you." "I wub wuu twoo~~" "I love you too!" "Tee hee." "I love you too."))
                           (alexandria:random-elt '("" "" "" "" ":)" "(ɔˆ ³(ˆ⌣ˆc)" "(ღ˘⌣˘ღ)" "(っ˘з(˘⌣˘ )" "(˘▼˘>ԅ( ˘⌣ƪ)"))))))

(define-command sandwich () (:documentation "Make a sandwich.")
  (if (auth-p (nick event))
      (respond event "~a: Sure thing, darling!" (nick event))
      (respond event "~a: Screw you." (nick event))))

(define-command sammich () (:documentation "You are stupid.")
  (sleep 2)
  (respond event ".. what?"))

(define-command roll (&optional (size "6") (times "1")) (:documentation "Roll a random number.")
  (cond
    ((or (string-equal size "infinity") (string-equal times "infinity"))
     (respond event "~ad~a: infinity" times size))
    ((string-equal times "mom")
     (if (string-equal size "your")
         (respond event "I would never hurt my creators!")
         (respond event "Down the hill rolls the fatty...")))
    ((or (string-equal times "joint") (string-equal size "joint"))
     (respond event "Don't do drugs, kids!"))
    (T
     (setf size (parse-integer size :junk-allowed T))
     (setf times (parse-integer times :junk-allowed T))
     (if (and size times)
         (respond event "~dd~d: ~d" times size (loop for i from 0 below times summing (1+ (random size))))
         (respond event "I don't know how to roll that.")))))

(define-command |8| (&rest |8|) (:documentation "\"Eight.\"")
  (declare (ignore |8|))
  (respond event "8."))

(define-command fortune (&rest what) (:documentation "Get the fortune about something.")
  (unless what (setf what (list (nick event))))
  (respond event "Fortune for ~{~a~^ ~}: Faggotry." what))

(define-command sex (&rest who) (:documentation "You really are incredibly pathetic.")
  (setf who (string-downcase (format NIL "~{~a~^ ~}" who)))
  (unless
      (loop for authd in (auth-users (server event))
         do (when (search (string-downcase authd) who)
              (respond event "...")
              (return T)))
    (respond event "What? Get the hell away from me you creep!")))
