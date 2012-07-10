;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defgeneric server-system (server))

(defgeneric server-package (server))

(defclass lisp-server ()

  ((system :initarg :system
           :accessor server-system
           :initform (error "Lisp server should recieve a :SYSTEM initarg."))

   (package :initform *package*
            :reader server-package)))
