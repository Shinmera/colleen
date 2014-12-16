#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defvar *debugger* NIL "Boolean indicating whether to invoke the debugger on an unhandled condition.")
(defvar *irc-message-limit* 512)
(defvar *privmsg-line-limit* 5)
(defvar *server-encoding* :UTF-8)
