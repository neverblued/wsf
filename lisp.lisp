;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defgeneric server-system (server))

(defgeneric server-package (server))

(defmacro with-server-package (server &body body)
  `(let ((*package* (server-package ,server)))
     ,@body))

(defclass lisp-server ()

  ((system :initarg :system
           :accessor server-system
           :initform (error "Lisp server should recieve a :SYSTEM initarg."))

   (package :initform *package*
            :reader server-package)))

(defmethod respond :around ((this lisp-server) request)
  (with-server-package this
    (call-next-method)))

(defun reload-system ()
  (awhen *server*
    (awith (server-system it)
      (asdf:load-system it))))
