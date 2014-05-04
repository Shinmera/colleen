#|
 This file is a part of Colleen
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defmacro with-module-storage ((&optional (module '*current-module*)) &body forms)
  `(let ((uc:*config* (storage (get-module ,module))))
     ,@forms))

(defun module-config-path (module)
  (merge-pathnames (string-downcase
                    (format NIL "~a.uc.~a"
                            (to-module-name module)
                            uc:*output-format*))
                   *config-directory*))

(defgeneric save-storage (module)
  (:documentation "")
  (:method ((module module))
    (let ((path (module-config-path module)))
      (v:info (to-module-name module) "Saving storage to ~a" path)
      (uc:save-configuration path :object (storage module)))))

(defgeneric load-storage (module)
  (:documentation "")
  (:method ((module module))
    (let ((uc:*config*)
          (path (module-config-path module)))
      (v:info (to-module-name module) "Loading storage from ~a" path)
      (if (uc:load-configuration path :if-does-not-exist NIL)
          (setf (storage module) uc:*config*)
          (v:info (to-module-name module) "Storage file not found.")))))
