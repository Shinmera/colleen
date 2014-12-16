#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defvar *debugger* NIL
  "Boolean indicating whether to invoke the debugger on an unhandled condition.")
(defvar *irc-message-limit* 512
  "Defines the maximum amount of characters in a IRC:PRIVMSG. This influences how long the messages can be before a warning is emitted and the message is possibly split into multiple parts.")
(defvar *privmsg-line-limit* 5
  "Defines the maximum amount of lines to send per IRC:PRIVMSG command.")
(defvar *server-encoding* :UTF-8
  "Sets the encoding to use with server connections.")
