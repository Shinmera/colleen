#|
This file is a part of Colleen
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.twitter
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.twitter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-oauth))

(define-module twitter ()
    ((%))
  (:documentation "Twitter module for streaming timelines or posting to a twitter account."))


