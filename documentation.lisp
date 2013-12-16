#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage :org.tymoonnext.colleen.doc
  (:use :cl :lquery :lquery-doc)
  (:nicknames :colleen-doc)
  (:export :build-documentation))

(in-package :org.tymoonnext.colleen.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object)))))

(defun build-documentation ()
  ($ (initialize (merge-pathnames "about-template.html" (asdf:system-source-directory :colleen))))
  (let ((template ($ "#template")))
    (let ((nodes (lquery-doc::documentate template :colleen :exclude '(:internal :method))))
      ($ "#docs" (empty) (append nodes)))
    (let ((nodes (lquery-doc::documentate template :irc :exclude '(:internal :method))))
      ($ "#cmd-docs" (empty) (append nodes)))
    (let ((nodes (lquery-doc::documentate template :events :exclude '(:internal :method))))
      ($ "#evt-docs" (empty) (append nodes))))
  ($ (write-to-file (merge-pathnames "about.html" (asdf:system-source-directory :colleen)))))
