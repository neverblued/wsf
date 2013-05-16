;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defgeneric server-pathname (docroot-server))

(defun from-docroot (server &rest relative-path-chunks)
  (awith (apply #'join relative-path-chunks)
    (join (server-pathname server)
          (if (begins-with? it "/") "" "/")
          it)))

(define-symbol-macro docroot
    (from-docroot *server*))

(defun docroot/ (&rest relative-path-chunks)
  (apply #'from-docroot *server* relative-path-chunks))

(defun text-docroot/ (&rest relative-path-chunks)
  (pathname-content (apply #'docroot/ relative-path-chunks)))

(defclass docroot-server (lisp-server)
  ((data-pathname-format :initarg :data-pathname-format
                         :accessor server-data-pathname-format
                         :initform "data/~a.lisp")))

(defmethod server-pathname ((server docroot-server))
  (system-directory (server-system server)))
