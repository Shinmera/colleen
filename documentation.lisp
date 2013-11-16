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
  (write-documentation :colleen
                       (merge-pathnames "about-template.html" (asdf:system-source-directory :colleen))
                       :output-file (merge-pathnames "about.html" (asdf:system-source-directory :colleen))
                       :exclude '(:internal)))
