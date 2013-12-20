#|
This file is a part of Colleen
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.shiritori
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.shiritori)

(define-module shiritori ()
  ((%games :initform (make-hash-table :test #'equalp) :accessor games))
  (:documentation "Lets you play a game of shiritori."))

(defparameter *romaji->hiragana-map*
  (let ((map (make-hash-table :test 'equal)))
    (dolist (var `(("a" . "あ") ("i" . "い") ("u" . "う") ("e" . "え") ("o" . "お")
                   ("ka" . "か") ("ki" . "き") ("ku" . "く") ("ke" . "け") ("ko" . "こ")
                   ("sa" . "さ") ("shi". "し") ("su" . "す") ("se" . "せ") ("so" . "そ")
                   ("ta" . "た") ("chi". "ち") ("tsu". "つ") ("te" . "て") ("to" . "と")
                   ("na" . "な") ("ni" . "に") ("nu" . "ぬ") ("ne" . "ね") ("no" . "の")
                   ("ha" . "は") ("hi" . "ひ") ("fu" . "ふ") ("he" . "へ") ("ho" . "ほ")
                   ("ma" . "ま") ("mi" . "み") ("mu" . "む") ("me" . "め") ("mo" . "も")
                   ("ya" . "や")               ("yu" . "ゆ")               ("yo" . "よ")
                   ("ra" . "ら") ("ri" . "り") ("ru" . "る") ("re" . "れ") ("ro" . "ろ")
                   ("wa" . "わ") ("wi" . "ゐ")               ("we" . "ゑ") ("wo" . "を")
                   ("n" . "ん")

                   ("ga" . "が") ("gi" . "ぎ") ("gu" . "ぐ") ("ge" . "げ") ("go" . "ご")
                   ("za" . "ざ") ("ji" . "じ") ("zu" . "ず") ("ze" . "ぜ") ("zo" . "ぞ")
                   ("da" . "だ")                            ("de" . "で") ("do" . "ど")
                   ("ba" . "ば") ("bi" . "び") ("bu" . "ぶ") ("be" . "べ") ("bo" . "ぼ")
                   ("pa" . "ぱ") ("pi" . "ぴ") ("pu" . "ぷ") ("pe" . "ぺ") ("po" . "ぽ")

                   ("kya" . "きゃ") ("kyu" . "きゅ") ("kyo" . "きょ")
                   ("sha" . "しゃ") ("shu" . "しゅ") ("sho" . "しょ")
                   ("cha" . "ちゃ") ("chu" . "ちゅ") ("cho" . "ちょ")
                   ("nya" . "にゃ") ("nyu" . "にゅ") ("nyo" . "にょ")
                   ("hya" . "ひゃ") ("hyu" . "ひゅ") ("hyo" . "ひょ")
                   ("mya" . "みゃ") ("myu" . "みゅ") ("myo" . "みょ")
                   ("rya" . "りゃ") ("ryu" . "りゅ") ("ryo" . "りょ")

                   ("gya" . "ぎゃ") ("gyu" . "ぎゅ") ("gyo" . "ぎょ")
                   ("ja"  . "じゃ") ("ju"  . "じゅ") ("jo"  . "じょ")
                   ("bya" . "びゃ") ("byu" . "びゅ") ("byo" . "びょ")
                   ("pya" . "ぴゃ") ("pyu" . "ぴゅ") ("pyo" . "ぴょ")))
      (setf (gethash (car var) map) (cdr var)))
    map))

(defun romaji->hiragana (string)
  (loop for romaji being the hash-keys of *romaji->hiragana-map*
     for hiragana being the hash-values of *romaji->hiragana-map*
     if (> (length romaji) 1)
     do (setf string (cl-ppcre:regex-replace-all romaji string hiragana)))
  (loop for romaji being the hash-keys of *romaji->hiragana-map*
     for hiragana being the hash-values of *romaji->hiragana-map*
     if (= (length romaji) 1)
     do (setf string (cl-ppcre:regex-replace-all romaji string hiragana)))
  string)

(defparameter *katakana->hiragana-map*
  (let ((map (make-hash-table :test 'equal)))
    (dolist (var `(("ア" . "あ") ("イ" . "え") ("ウ" . "う") ("エ" . "え") ("オ" . "お")
                   ("カ" . "か") ("キ" . "き") ("ク" . "く") ("ケ" . "け") ("コ" . "こ")
                   ("サ" . "さ") ("シ" . "し") ("ス" . "す") ("セ" . "せ") ("ソ" . "そ")
                   ("タ" . "た") ("チ" . "ち") ("ツ" . "つ") ("テ" . "て") ("ト" . "と")
                   ("ナ" . "な") ("ニ" . "に") ("ヌ" . "ぬ") ("ネ" . "ね") ("ノ" . "の")
                   ("ハ" . "は") ("ヒ" . "ひ") ("フ" . "ふ") ("ヘ" . "へ") ("ホ" . "ほ")
                   ("マ" . "ま") ("ミ" . "み") ("ム" . "む") ("メ" . "め") ("モ" . "も")
                   ("ヤ" . "や")               ("ユ" . "ゆ")              ("ヨ" . "よ")
                   ("ラ" . "ら") ("リ" . "り") ("ル" . "る") ("レ" . "れ") ("ロ" . "ろ")
                   ("ワ" . "わ") ("ヰ" . "ゐ")               ("ヱ" . "ゑ") ("ヲ" . "を")
                   ("ン" . "ん")

                   ("ガ" . "が") ("ギ" . "ぎ") ("グ" . "ぐ") ("ゲ" . "げ") ("ゴ" . "ご")
                   ("ザ" . "ざ") ("ジ" . "じ") ("ズ" . "ず") ("ゼ" . "ぜ") ("ゾ" . "ぞ")
                   ("ダ" . "だ")                            ("デ" . "で") ("ド" . "ど")
                   ("バ" . "ば") ("ビ" . "び") ("ブ" . "ぶ") ("ベ" . "べ") ("ボ" . "ぼ")
                   ("パ" . "ぱ") ("ピ" . "ぴ") ("プ" . "ぷ") ("ペ" . "ぺ") ("ポ" . "ぽ")

                   ("ャ" . "ゃ") ("ュ" . "ゅ") ("ョ" . "ょ")))
      (setf (gethash (aref (car var) 0) map) (aref (cdr var) 0)))
    map))

(defun katakana->hiragana (string)
  "Primitively convert katakana to hiragana."
  (loop with new-string = (make-string (length string))
     for char across string
     for i from 0
     do (setf (aref new-string i) (or (gethash char *katakana->hiragana-map*) char))
     finally (return new-string)))

(define-command shiritori () (:documentation "Start a game of shiritori.")
  (setf (gethash (format NIL "~a/~a" (name (server event)) (channel event)) (games module)) (list ""))
  (respond event "Game started! Any future messages until the game ends will be considered to be part of the Shiritori."))

(defun last-kana (string)
  (let ((length (length string)))
    (if (= 1 length)
        string
        (if (and (> length 1)
                 (find (subseq string (- length 1)) '("ゃ" "ゅ" "ょ" "ャ" "ュ" "ョ") :test #'string=))
            (subseq string (- length 2))
            (subseq string (- length 1))))))

(define-handler (privmsg-event event) ()
  (let* ((game-id (format NIL "~a/~a" (name (server event)) (channel event)))
         (game (gethash game-id (games module))))
    (when game
      (let* ((message (katakana->hiragana (romaji->hiragana (message event))))
             (last-player (car game))
             (used-words (cdr game))
             (last-kana (when used-words (last-kana (message event)))))
        (cond
          ((string-equal last-player (nick event))
           (respond event "~a said something twice in a row and loses!" (nick event))
           (remhash game-id (games module)))
          ((char= (aref message (1- (length message))) (aref "ん" 0))
           (respond event "~a loses due to the word ending on ん!" (nick event))
           (remhash game-id (games module)))
          ((find message used-words :test #'string=)
           (respond event "~a repeated a word and thus loses!")
           (remhash game-id (games module)))
          ((and last-kana (not (string= message last-kana :end1 (length last-kana))))           
           (respond event "~a's word does not begin with ~a. Game over!" (nick event) last-kana)
           (remhash game-id (games module)))
          (T
           (setf (car game) (nick event)
                 (cdr game) (append used-words (list message)))))))))
