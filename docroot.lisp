;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defgeneric server-pathname (docroot-server))

(defun from-docroot (server &rest relative-path-chunks)
  (join (server-pathname server) "/" (apply #'join relative-path-chunks)))

(defclass docroot-server (lisp-server) ())

(defmethod server-pathname ((server docroot-server))
  (system-directory (server-system server)))
