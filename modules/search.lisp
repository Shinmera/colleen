#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.search
  (:use :cl :colleen :lquery)
  (:shadow :search))
(in-package :org.tymoonnext.colleen.mod.search)

(define-module search () ()
  (:documentation "Perform searches on various sites."))

(define-group search :documentation "Perform a search on a variety of sites.")

(define-command (search lmgtfy) (&rest query) (:documentation "Return a \"Let me google that for you\" link.")
  (respond event (shorten-url (format NIL "http://lmgtfy.com/?q=~{~a~^ ~}" query))))

(define-command (search google) (&rest query) (:documentation "Query google and return the first search result link.")
  (let ((lquery:*lquery-master-document*))
    ($ (initialize (drakma:http-request (format NIL "http://www.google.com/search?q=~{~a~^+~}" query)) :type :HTML))
    (let ((node ($ "#res #search li .s" (eq 0))))
      (respond event "http://~a ~a" 
               ($ node ".kv cite" (text) (node))
               (cl-ppcre:regex-replace-all "\\n" ($ node ".st" (text) (node)) "")))))

(define-command (search wikipedia) (&rest query) (:documentation "Search wikipedia and return the first paragraph of a matching page.")
  (mediawiki-search-wrap event query "http://en.wikipedia.org/wiki/" "http://en.wikipedia.org/w/api.php" 0))

(define-command (search wiktionary) (&rest query) (:documentation "Search wiktionary and return the first paragraph of a matching page.")
  (mediawiki-search-wrap event query "http://en.wiktionary.org/wiki/" "http://en.wiktionary.org/w/api.php" 1))

(define-command (search ed) (&rest query) (:documentation "Search encyclopediadramatica and return the first paragraph of a matching page.")
  (mediawiki-search-wrap event query "http://encyclopediadramatica.es/" "http://encyclopediadramatica.es/api.php" 0))

(define-command (search cliki) (&rest query) (:documentation "Search CLiki.net and return the first paragraph of a matching page.")
  )

(define-command (search kanjidamage) (symbol) (:documentation "Return information about a kanji symbol crawled from Kanjidamage.com")
  )

(define-command (search jigen) (symbol) (:documentation "Search Jigen.net and return information about a symbol.")
  )

(define-command (search clhs) (&rest query) (:documentation "Search the Common Lisp Hyperspec and return the short explanation.")
  )

(define-command (search shorten) (&rest address) (:documentation "Create a shortened URL through bit.ly .")
  (let ((short (shorten-url (format NIL "~{~a~^ ~}" address))))
    (if short
        (respond event short)
        (respond event "Url-shortening failed!"))))

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
                   :external-format-out :utf-8 :external-format-in :utf-8 :user-agent :chrome) 
                  :external-format :utf-8))))
      (values (g data :url :data)
              (g data :hash :data)
              (g data :long--url :data)
              (g data :global--hash :data)
              data))))









