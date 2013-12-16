#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.search
  (:use :cl :colleen :events :lquery)
  (:shadow :search))
(in-package :org.tymoonnext.colleen.mod.search)

(defun drakma-utf8 (url &rest params)
  (apply #'drakma:http-request url :external-format-in :utf-8 :external-format-out :utf-8 :user-agent :chrome :preserve-uri T params))

(define-module search () ()
  (:documentation "Perform searches on various sites."))

(define-group search :documentation "Perform a search on a variety of sites.")

(define-command (search lmgtfy) (&rest query) (:documentation "Return a \"Let me google that for you\" link.")
  (respond event (shorten-url (format NIL "http://lmgtfy.com/?q=~{~a~^ ~}" query))))

(define-command (search google) (&rest query) (:documentation "Query google and return the first search result link.")
  (multiple-value-bind (url description) (google-term (format NIL "~{~a~^ ~}" query))
    (respond event "~a : ~a" url description)))

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

(define-command (search wiktionary) (&rest query) (:documentation "Search wiktionary.")
  (mediawiki-search-wrap event query "http://en.wiktionary.org/wiki/" "http://en.wiktionary.org/w/api.php" 1))

(define-command (search ed) (&rest query) (:documentation "Search encyclopediadramatica, the number one wiki for internet drama.")
  (mediawiki-search-wrap event query "http://encyclopediadramatica.es/" "http://encyclopediadramatica.es/api.php" 0))

(define-command (search kanjidamage) (&rest query) (:documentation "Return information about a kanji symbol crawled from Kanjidamage.com")
  (setf query (format NIL "~{~a~^ ~}" query))
  (let ((lquery:*lquery-master-document*))
    (multiple-value-bind (html status headers uri) (drakma:http-request "http://kanjidamage.com/kanji/search" 
                                                                        :parameters `(("q" . ,query) ("utf8" . "✓")) 
                                                                        :external-format-in :utf-8 :external-format-out :utf-8)
      (declare (ignore status headers))
      ($ (initialize html :type :HTML))
      (respond event "~a : ~a ~a - ~a" 
               uri
               ($ "h1 .kanji_character" (text) (node))
               ($ "h1 .translation" (text) (node))
               (cl-ppcre:regex-replace-all "\\s\\s" ($ ".span12 .definition p" (text) (node)) " ")))))

(define-command (search jigen) (symbol &optional n) (:documentation "Search Jigen.net, a kanji character database.")
  (let ((lquery:*lquery-master-document*))
    ($ (initialize (drakma-utf8 (format NIL "http://jigen.net/data/~a?type2=1&rs=-1" (drakma:url-encode symbol :utf-8))) :type :HTML))
    (let ((results ($ "#main .ka_list li")))
      (if (and (cdr results) (not n))
          (respond event "Mathing pages (please specify nubmer): ~{~a~^, ~}" ($ results "a" (text)))
          (let ((page ($ results (eq (or n 0)) "a" (attr :href) (node))))
            ($ (initialize (drakma-utf8 page) :type :HTML))
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
    ($ (initialize (drakma:http-request "http://www.cliki.net/site/search" :parameters `(("query" . ,query)) :external-format-in :utf-8) :type :HTML))
    (let ((results ($ "#content-area li")))
      (if results
          (let ((page (format NIL "http://www.cliki.net~a" ($ results (eq 0) "a" (attr :href) (node)))))
            ($ (initialize (drakma-utf8 page) :type :HTML))
            (cl-ppcre:register-groups-bind (html NIL) ("<div id=\"article\">(.*?)<" ($ "#article" (serialize) (node)))
              ($ (initialize (format NIL "<html><head></head><body>~a</body></html>" html) :type :HTML))
              (respond event "~a : ~a" page ($ "body" (text) (node)))))
          (respond event "Nothing found for ~a." query)))))

(define-command (search clhs) (&rest query) (:documentation "Search the Common Lisp Hyperspec and return the short explanation.")
  (let ((lquery:*lquery-master-document*)
        (url (google-term (format NIL "clhs+Body+~{~a~^+~}" query))))
    ($ (initialize (drakma:http-request url) :type :HTML))
    (respond event "~a ~a" ($ "body>a" (eq 0) (text) (node)) url)))

(define-command (search shorten) (&rest address) (:documentation "Create a shortened URL through bit.ly .")
  (let ((short (shorten-url (format NIL "~{~a~^ ~}" address))))
    (if short
        (respond event short)
        (respond event "Url-shortening failed!"))))

(defun google-term (term)
  (let ((lquery:*lquery-master-document*))
    ($ (initialize (drakma:http-request "http://www.google.com/search" 
                                        :parameters `(("q" . ,term))
                                        :external-format-out :utf-8
                                        :external-format-in :utf-8) :type :HTML))
    (let ((node ($ "#res #search li" (eq 0))))
      (values (cl-ppcre:register-groups-bind (url) ("\\?q=(.*?)&" ($ node "h3 a" (attr :href) (node))) url)
              (cl-ppcre:regex-replace-all "\\n" ($ node ".s .st" (text) (node)) "")))))

(defun mediawiki-search-wrap (event query base api &optional (section 0))
  (let ((result (mediawiki-search query base api section)))
    (if result 
        (respond event result)
        (respond event "Nothing found for ~{~a~^ ~}" query))))

(defun mediawiki-search (query page-root api-root &optional (section 0))
  (let* ((wiki:*wiki-api* api-root)
         (title (cdr (assoc :TITLE (first (wiki:wiki-search (format NIL "~{~a~^ ~}" query) :limit 1 :what "text"))))))
    (when title
      (progn
        (setf title (cl-ppcre:regex-replace-all " " title "_"))
        (let ((data (wiki:wiki-parse :page title :section section)))
          (v:info :AAA data)
          ($ (initialize (format NIL "<html><head></head><body>~a</body></html>" 
                                 (cl-ppcre:regex-replace-all "xml:" data "")) :type :HTML))
          (format NIL "~a~a : ~a" page-root title ($ "p" (node) (text) (node))))))))

(defun shorten-url (url)
  (labels ((g (data &rest path)
             (if path 
                 (cdr (assoc (car path) (apply #'g data (cdr path)))) 
                 data)))
    (let ((data (json:decode-json-from-string
                 (flexi-streams:octets-to-string
                  (drakma:http-request
                   "http://api.bitly.com/v3/shorten"
                   :parameters `(("login" . "o_2vsdcdup6q")
                                 ("apiKey" . "R_37fc5c0f8bc59052587bee13c2257c4f")
                                 ("longUrl" . ,url))
                   :external-format-out :utf-8
                   :external-format-in :utf-8) 
                  :external-format :utf-8))))
      (values (g data :url :data)
              (g data :hash :data)
              (g data :long--url :data)
              (g data :global--hash :data)
              data))))









