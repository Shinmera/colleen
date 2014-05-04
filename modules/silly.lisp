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

(defparameter *save-file* (merge-pathnames "silly.json" (merge-pathnames "config/" (asdf:system-source-directory :colleen))))
(defparameter *thanks-match* (cl-ppcre:create-scanner "[Tt]hanks[,]? ([a-zA-Z]+)$"))
(defparameter *bless-match* (cl-ppcre:create-scanner "[Bb]less you[,]? ([a-zA-Z]+)$"))

(defun cut-to-first-vocal (string)
  (loop for i from 0 below (length string)
        until (find (aref string i) '(#\a #\e #\i #\o #\u))
        finally (return (subseq string i))))

(define-group silly :documentation "Manage the silly module.")

(define-command (silly activate) (&optional channel server) (:authorization T :documentation "Activate the silly responses for the channel.")
  (unless channel (setf channel (channel event)))
  (unless server (setf server (name (server event))))
  (pushnew (format NIL "~a/~a" server channel) (uc:config-tree :active-in) :test #'string-equal)
  (respond event "Activated silliness."))

(define-command (silly deactivate) (&optional channel server) (:authorization T :documentation "Deactivate the silly responses for the channel.")
  (unless channel (setf channel (channel event)))
  (unless server (setf server (name (server event))))
  (setf (uc:config-tree :active-in)
        (delete (format NIL "~a/~a" server channel) (uc:config-tree :active-in) :test #'string-equal))
  (respond event "Deactivated silliness."))

(define-handler (privmsg-event event) ()
  (when (member (format NIL "~a/~a" (name (server event)) (channel event)) (uc:config-tree :active-in) :test #'string-equal)
    (let ((message (string-downcase (message event))))
      (cl-ppcre:register-groups-bind (name) (*thanks-match* message)
        (sleep 2)
        (respond event "...Th~a" (cut-to-first-vocal name)))
      (cl-ppcre:register-groups-bind (name) (*bless-match* message)
        (sleep 2)
        (respond event "...Bl~a" (cut-to-first-vocal name)))
      
      (when (cl-ppcre:scan "^colleen: (and )?(oh )?i (love|luv|wub) (you|u|wu|wuu|wo|woo)( too)?( as well)?( (so|very|too)( much)?)?$" message)
        (respond event (format NIL "~a: ~a ~a" 
                               (nick event) 
                               (alexandria:random-elt '("I love you too." "Aww!" "Oh you~~" "Haha, oh you." "I wub wuu twoo~~" "I love you too!" "Tee hee." "I love you too."))
                               (alexandria:random-elt '("" "" "" "" ":)" "(ɔˆ ³(ˆ⌣ˆc)" "(ღ˘⌣˘ღ)" "(っ˘з(˘⌣˘ )" "(˘▼˘>ԅ( ˘⌣ƪ)")))))

      (when (or (cl-ppcre:scan "(i|you|he|she|it|we|they)( all)? know(s?) now" message)
                (cl-ppcre:scan "now (i|you|he|she|it|we|they)( all)? know(s?)" message))
        (sleep (/ (random 10) 5))
        (respond event (alexandria:random-elt '("...now we know." "... oh yeah we know now." "NOW WE KNOW!" "NOW WE KNOOOW!!" "...yeah that's good. Now we know."))))
      (when (cl-ppcre:scan "(what|who) did you expect" message)
        (sleep (/ (random 10) 5))
        (respond event "Who were you expecting.... the easter bunny>"))

      (when (cl-ppcre:scan "that (was|is) the plan" message)
        (sleep (/ (random 20) 10))
        (respond event "...to give you a boner.")
        (sleep (/ (random 20) 7))
        (respond event "And you got one!"))

      (when (cl-ppcre:scan "(/burn)|(sick burn)|(o+h+ burn)" message)
        (sleep (/ (random 10) 20))
        (respond event "OOOOOOHH SICK BURN!!"))

      (when (cl-ppcre:scan "muffin" message)
        (sleep (/ (random 10) 20))
        (respond event "Aw yeeeee. Muffiiins."))

      (when (cl-ppcre:scan "i'm batman" message)
        (sleep (/ (random 10) 5))
        (respond event "I'M BATMAN."))

      (when (cl-ppcre:scan "kill myself" message)
        (sleep (/ (random 10) 10))
        (respond event "DO IT FGT!"))

      (when (cl-ppcre:scan "yukkuri|take it easy|ゆっくり" message)
        (sleep (/ (random 20) 10))
        (if (< 1 (random 20))
            (respond event "ゆっくりしていってね！")
            (let ((*privmsg-line-limit* 14))
              (respond event "　　 _,,....,,_　 ＿人人人人人人人人人人人人人人人＿
-''\":::::::::::::｀''＞　　　ゆっくりしていってね！！！　　　＜
ヽ:::::::::::::::::::::￣^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^￣
　|::::::;ノ´￣＼:::::::::::＼_,. -‐ｧ　　　　　＿_　　 _____　　 ＿_____
　|::::ﾉ　　　ヽ､ヽr-r'\"´　　（.__　　　　,´　_,, '-´￣￣｀-ゝ 、_ イ、
_,.!イ_　　_,.ﾍｰｧ'二ﾊ二ヽ､へ,_7　　　'r ´　　　　　　　　　　ヽ、ﾝ、
::::::rｰ''7ｺ-‐'\"´　 　 ;　 ',　｀ヽ/｀7　,'＝=─-　　　 　 -─=＝',　i
r-'ｧ'\"´/　 /!　ﾊ 　ハ　 !　　iヾ_ﾉ　i　ｲ　iゝ、ｲ人レ／_ルヽｲ i　|
!イ´ ,' |　/__,.!/　V　､!__ﾊ　 ,'　,ゝ　ﾚﾘｲi (ﾋ_] 　　 　ﾋ_ﾝ ).| .|、i .||
`! 　!/ﾚi'　(ﾋ_] 　　 　ﾋ_ﾝ ﾚ'i　ﾉ　　　!Y!\"\"　 ,＿__, 　 \"\" 「 !ﾉ i　|
,'　 ﾉ 　 !'\"　 　 ,＿__,　 \"' i .ﾚ'　　　　L.',.　 　ヽ _ﾝ　　　　L」 ﾉ| .|
　（　　,ﾊ　　　　ヽ _ﾝ　 　人! 　　　　 | ||ヽ、　　　　　　 ,ｲ| ||ｲ| /
,.ﾍ,）､　　）＞,､ _____,　,.イ　 ハ　　　　レ ル｀ ー--─ ´ルﾚ　ﾚ´"))))

      (when (cl-ppcre:scan "how ((is (this|that) (even )?possible)|the hell|in the world)" message)
        (sleep (/ (random 20) 10))
        (respond event "NANO MACHINES, SON"))

      (when (cl-ppcre:scan "power glove" message)
        (sleep (/ (random 20) 10))
        (respond event "I love the powerglove... it's so bad."))

      (cl-ppcre:register-groups-bind (thing) ("distracted by (.+)" message)
        (sleep (/ (random 10) 20))
        (respond event "The ~a ruse was a.........." thing)
        (sleep (/ (random 10) 10))
        (respond event "DISTACTION")))))

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
         (respond event "I would never hurt my mom!")
         (respond event "Down the hill rolls the fatty...")))
    ((or (string-equal times "joint") (string-equal size "joint"))
     (respond event "Don't do drugs, kids!"))
    ((string-equal size "over")
     (respond event "No."))
    (T
     (setf size (parse-integer size :junk-allowed T))
     (setf times (parse-integer times :junk-allowed T))
     (if (and size times)
         (respond event "~dd~d: ~d" times size (loop for i from 0 below times summing (1+ (random size))))
         (respond event "I don't know how to roll that.")))))

(define-command |8| (&rest |8|) (:documentation "\"Eight.\"")
  (declare (ignore |8|))
  (respond event "Eight."))

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
    (respond event "Get the hell away from me you creep!")))

(define-command kill (who) (:authorization T :documentation "Kill someone.")
  (respond event "~a: Bang!" who)
  (irc:kick (channel event) who :reason "You're dead."))

(define-command thanks () (:documentation "Thanks you." :eventvar event)
  (respond event "Thanks, ~a" (nick event))
  (sleep 2)
  (respond event "...Th~a" (cut-to-first-vocal (nick event))))
