#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.search
  (:use :cl :colleen :events :lquery :split-sequence)
  (:shadow :search))
(in-package :org.tymoonnext.colleen.mod.search)

(defun drakma-utf8 (url &rest params)
  (apply #'drakma:http-request url :external-format-in :utf-8 :external-format-out :utf-8 :user-agent :chrome :preserve-uri T params))

(defun trunc-text (text &optional (maxchars 200))
  (let ((text (string-trim '(#\Newline #\Space #\Linefeed #\Return) text)))
    (if (> (length text) maxchars)
        (concatenate 'string (subseq text 0 maxchars) "...")
        text)))

(define-module search () ()
  (:documentation "Perform searches on various sites."))

(define-command clhs (&rest query) (:documentation "Look up on the Common Lisp Hyperspec.")
  (if (= (length query) 0)
      (respond event "The Common Lisp Hyperspec http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")
      (colleen:dispatch
       (make-instance 'command-event
                      :server (server event)
                      :arguments (arguments event)
                      :prefix (prefix event)
                      :message (format NIL "search ~a" (message event))))))

(define-group search :documentation "Perform a search on a variety of sites.")

(define-command (search lmgtfy) (&rest query) (:documentation "Return a \"Let me google that for you\" link.")
  (respond event (shorten-url (format NIL "http://lmgtfy.com/?q=~{~a~^ ~}" query))))

(define-command (search google) (&rest query) (:documentation "Query google and return the first search result link.")
  (multiple-value-bind (url description) (google-term (format NIL "~{~a~^ ~}" query))
    (respond event "~a : ~a" url (trunc-text description))))

(define-command (search image) (&rest query) (:documentation "Query google images and return the link to the first image in the results.")
  (setf query (format NIL "~{~a~^ ~}" query))
  (let* ((stream
          (drakma:http-request "https://ajax.googleapis.com/ajax/services/search/images" :want-stream T
                               :parameters `(("q" . ,query) ("v" . "1.0"))
                               :external-format-out :utf-8
                               :external-format-in :utf-8))
         (data (json:decode-json stream))
         (results (cdr (assoc :results (cdr (assoc :response-data data))))))
    (close stream)
    (if results
        (respond event "~a : ~a" 
                 (cdr (assoc :url (first results)))
                 (cdr (assoc :title-no-formatting (first results))))
        (respond event "No results found for ~a." query))))

(define-command (search wikipedia) (&rest query) (:documentation "Search wikipedia.")
  (mediawiki-search-wrap event query "http://en.wikipedia.org/wiki/" "http://en.wikipedia.org/w/api.php" 0))

(define-command (search wikipedia-de) (&rest query) (:documentation "Search the German wikipedia.")
  (mediawiki-search-wrap event query "http://de.wikipedia.org/wiki/" "http://de.wikipedia.org/w/api.php" 0))

(define-command (search wiktionary) (&rest query) (:documentation "Search wiktionary.")
  (mediawiki-search-wrap event query "http://en.wiktionary.org/wiki/" "http://en.wiktionary.org/w/api.php" 1))

(define-command (search ed) (&rest query) (:documentation "Search encyclopediadramatica, the number one wiki for internet drama.")
  (mediawiki-search-wrap event query "http://encyclopediadramatica.es/" "http://encyclopediadramatica.es/api.php" 0))

(define-command (search touhou) (&rest query) (:documentation "Search en.touhouwiki.net.")
  (mediawiki-search-wrap event query "http://en.touhouwiki.net/wiki/" "http://en.touhouwiki.net/api.php" 0 "title"))

(define-command (search kanjidamage) (&rest query) (:documentation "Return information about a kanji symbol crawled from Kanjidamage.com")
  (setf query (format NIL "~{~a~^ ~}" query))
  (let ((lquery:*lquery-master-document*))
    (multiple-value-bind (html status headers uri) (drakma-utf8 "http://kanjidamage.com/kanji/search" 
                                                                :parameters `(("q" . ,query) ("utf8" . "✓")))
      (declare (ignore status headers))
      ($ (initialize html))
      (respond event "~a : ~a ~a - ~a" 
               uri
               ($ "h1 .kanji_character" (text) (node))
               ($ "h1 .translation" (text) (node))
               (trunc-text (cl-ppcre:regex-replace-all "\\s\\s" ($ ".span12 .definition p" (text) (node)) " "))))))

(define-command (search jigen) (symbol &optional n) (:documentation "Search Jigen.net, a kanji character database.")
  (let ((lquery:*lquery-master-document*))
    ($ (initialize (drakma-utf8 (format NIL "http://jigen.net/data/~a?type2=1&rs=-1" (drakma:url-encode symbol :utf-8)))))
    (let ((results (coerce ($ "#main .ka_list li") 'list)))
      (if (and (cdr results) (not n))
          (respond event "Mathing pages (please specify nubmer): ~{~a~^, ~}" ($ results "a" (text)))
          (let ((page ($ results (eq (or n 0)) "a" (attr :href) (node))))
            ($ (initialize (drakma-utf8 page)))
            ($ "sup" (remove))
            (let* ((dds ($ "#kjid dl dd"))
                   (add (- (length dds) 7)))
              (respond event "~a : Readings: ~{~a~^, ~}; Composition: ~{~a~}; Pronunciation: ~{~a~^, ~}; Display: ~a; Variants: ~{~a~^, ~}" 
                       page
                       ($ dds (eq (+ add 0)) "li" (text))
                       ($ dds (eq (+ add 2)) (text))
                       ($ dds (eq (+ add 3)) "li" (text))
                       ($ dds (eq (+ add 4)) (text) (node))
                       ($ dds (eq (+ add 5)) "li" (text)))))))))

(define-command (search cliki) (&rest query) (:documentation "Search CLiki.net, a wiki dedicated to open source lisp projects and such..")
  (setf query (format NIL "~{~a~^+~}" query))
  (let ((lquery:*lquery-master-document*))
    ($ (initialize (drakma-utf8 "http://www.cliki.net/site/search" :parameters `(("query" . ,query)))))
    (let ((results ($ "#content-area li")))
      (if (= 0 (length results))
          (let ((page (format NIL "http://www.cliki.net~a" ($ results (eq 0) "a" (attr :href) (node)))))
            ($ (initialize (drakma-utf8 page)))
            (cl-ppcre:register-groups-bind (html NIL) ("<div id=\"article\">(.*?)<" ($ "#article" (serialize) (node)))
              ($ (initialize (format NIL "<html><head></head><body>~a</body></html>" html)))
              (respond event "~a : ~a" page (trunc-text ($ "body" (text) (node))))))
          (respond event "Nothing found for ~a." query)))))

(define-command (search clhs) (&rest query) (:documentation "Search the Common Lisp Hyperspec and return the short explanation.")
  (let ((lquery:*lquery-master-document*)
        (url (google-term (format NIL "clhs+Body+~{~a~^+~}" query))))
    (if url
        (progn
          ($ (initialize (drakma:http-request url)))
          (respond event "~a ~a" ($ "body>a" (eq 0) (text) (node)) url))
        (respond event "Nothing found."))))

(define-command (search shorten) (&rest address) (:documentation "Create a shortened URL through bit.ly .")
  (let ((short (shorten-url (format NIL "~{~a~^ ~}" address))))
    (if short
        (respond event short)
        (respond event "Url-shortening failed!"))))

(define-command (search steam) (&rest query) (:documentation "Search the steam store for games.")
  (setf query (format NIL "~{~a~^ ~}" query))
  (let ((lquery:*lquery-master-document*))
    ($ (initialize (drakma-utf8 "http://store.steampowered.com/search/" :parameters `(("term" . ,query)) :method :post)))
    (let ((results ($ ".search_result_row")))
      (if (< 0 (length results))
          (let ((page ($ results (eq 0) (attr :href) (node))))
            ($ (initialize (drakma-utf8 page)))
            (respond event "~a : ~a ~a"
                     (trunc-text ($ ".apphub_AppName" (text) (node)))
                     (trunc-text ($ ".game_description_snippet" (text) (node)))
                     page))
          (respond event "Nothing found for ~a." query)))))

(define-command (search chinese-etymology) (hanzi) (:documentation "Search Chinese Etymology, a repository for the composition of Chinese characters.")
  (destructuring-bind (cog phon label shuowen)
      ($ (initialize 
          (drakma-utf8 "http://www.chineseetymology.org/CharacterEtymology.aspx" 
                       :parameters `(("characterInput" . ,hanzi)
                                     ("submitButton1" . "Etymology"))))
        (combine "#charCog" "#charPhon" "#etymologyLabel" "#ShuoWen")
        (map #'(lambda (nodes) (mapcar #'(lambda (node) (aref (lquery-funcs:text node) 0)) nodes)))
        (node))
    (respond event "Label: ~a Semantic (<X|) component: ~:[~a~;~*NONE~]; phonetic (|Y>) component: ~:[~a~;~*NONE~]"
             label (string= "" cog) cog (string= "" phon) phon)
    (unless (or (string-equal "" shuowen)
                (string-equal "none" shuowen))
      (respond event "Extra credit: ~a" shuowen))))

(defun google-term (term)
  (let ((lquery:*lquery-master-document*))
    ($ (initialize (drakma:http-request "http://www.google.com/search" 
                                        :parameters `(("q" . ,term))
                                        :external-format-out :utf-8
                                        :external-format-in :utf-8)))
    (let ((node ($ "#res #search li")))
      (unless (= 0 (length node))
        (values (cl-ppcre:register-groups-bind (url) ("\\?q=(.*?)&" ($ node "h3 a" (attr :href) (node))) url)
                (cl-ppcre:regex-replace-all "\\n" ($ node ".s .st" (text) (node)) ""))))))

(defun mediawiki-search-wrap (event query base api &optional (section 0) (what "text"))
  (let ((result (mediawiki-search query base api section what)))
    (if result 
        (respond event result)
        (respond event "Nothing found for ~{~a~^ ~}" query))))

(defun mediawiki-search (query page-root api-root &optional (section 0) (what "text"))
  (let* ((wiki:*wiki-api* api-root)
         (title (cdr (assoc :TITLE (first (wiki:wiki-search (format NIL "~{~a~^ ~}" query) :limit 1 :what what))))))
    (when title
      (setf title (cl-ppcre:regex-replace-all " " title "_"))
      (let ((data (wiki:wiki-parse :page title :section section)))
        ($ (initialize (cl-ppcre:regex-replace-all "xml:" data "")))
        (format NIL "~a~a : ~a" page-root title (trunc-text (first (split-sequence #\Newline ($ "p:first-only" (text) (node))))))))))

(defun shorten-url (url)
  (labels ((g (data &rest path)
             (if path 
                 (cdr (assoc (car path) (apply #'g data (cdr path)))) 
                 data)))
    (let ((data (json:decode-json-from-string
                 (flexi-streams:octets-to-string
                  (drakma-utf8 "http://api.bitly.com/v3/shorten"
                               :parameters `(("login" . "o_2vsdcdup6q")
                                             ("apiKey" . "R_37fc5c0f8bc59052587bee13c2257c4f")
                                             ("longUrl" . ,url))) 
                  :external-format :utf-8))))
      (values (g data :url :data)
              (g data :hash :data)
              (g data :long--url :data)
              (g data :global--hash :data)
              data))))









