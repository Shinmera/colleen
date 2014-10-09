#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defvar *priority-names*
  (loop with map = (make-hash-table)
        for (name val) in `((:FIRST ,most-positive-fixnum)
                            (:PREPROCESS 200) (:BEFORE 100)
                            (:MAIN 0) (:STANDARD 0) (:DEFAULT 0)
                            (:AFTER -100) (:POSTPROCESS -200)
                            (:LAST ,most-negative-fixnum))
        do (setf (gethash name map) val)
        finally (return map))
  "Hash table mapping arbitrary names to event priorities.
Defined by default are :FIRST :PREPROCESS :BEFORE :MAIN :STANDARD
:DEFAULT :AFTER :POSTPROCESS :LAST.")
(defvar *priority-nums*
  (loop with map = (make-hash-table)
        for name being the hash-keys of *priority-names*
        for num being the hash-values of *priority-names*
        do (setf (gethash num map) name)
        finally (return map))
  "Reverse mapping table for event priorities to names.
See *PRIORITY-NAMES*.")

(defun priority-name (num)
  (gethash num *priority-nums* num))

(defun priority-num (name)
  (gethash name *priority-names* name))

(defgeneric (setf priority-name) (num name)
  (:documentation "Set a priority name to a new priority number.")
  (:method ((num real) (name symbol))
    (setf (gethash name *priority-names*) num
          (gethash num *priority-nums*) name)))

(defgeneric (setf priority-num) (name num)
  (:documentation "Set a priority number to a new priority name.")
  (:method ((name symbol) (num real))
    (setf (gethash name *priority-names*) num
          (gethash num *priority-nums*) name)))
