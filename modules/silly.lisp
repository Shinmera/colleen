#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.silly
  (:use :cl :colleen))
(in-package :org.tymoonnext.colleen.mod.silly)

(define-module silly () ()
  (:documentation "Silly things."))

(define-handler (privmsg-event event) ()
  (when (cl-ppcre:scan "^colleen: (and )?(oh )?i ((love)|(luv)|(wub)) ((you)|(u)|(wu)|(wuu))( too)?( as well)?( ((so)|(very)|(too))( much)?)?$" (string-downcase (message event)))
    (respond event (format NIL "~a: ~a ~a" 
                           (nick event) 
                           (alexandria:random-elt '("I love you too." "Aww!" "Oh you~~" "Haha, oh you." "I wub wuu twoo~~" "I love you too!" "Tee hee." "I love you too."))
                           (alexandria:random-elt '("" "" "" "" "" "" "" "" "" ":)" "(ɔˆ ³(ˆ⌣ˆc)" "(ღ˘⌣˘ღ)" "(っ˘з(˘⌣˘ )" "(˘▼˘>ԅ( ˘⌣ƪ)"))))))

(define-command sandwich () (:documentation "Make a sandwich.")
  (if (auth-p (nick event))
      (respond event "~a: Sure thing, darling!" (nick event))
      (respond event "~a: Screw you." (nick event))))

(define-command sammich () (:documentation "You are stupid.")
  (sleep 2)
  (respond event ".. what?"))
