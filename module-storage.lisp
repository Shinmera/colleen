#|
 This file is a part of Colleen
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)

(defmacro wrap-function (function-symbol)
  (let ((args (function-arguments (symbol-function function-symbol))))
    `(defun ,(intern (string function-symbol) :colleen) ,args
       (let ((uc:*config* (storage (get-module (get-current-module-name)))))
         (,function-symbol ,@(build-lambda-call args))))))

(wrap-function uc:access)
(wrap-function uc:config-tree)
(wrap-function uc:save-configuration)
(wrap-function uc:load-configuration)

