;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defclass router ()
  ((routes :accessor routes :initform nil)))

(defvar *router* nil "Текущий маршрутизатор")

(defmacro with-router (router &body body)
  "Создать окружение маршрутизатора"
  `(let ((*router* ,router))
     (declare (router *router*))
     ,@body))

(defmethod respond ((router router) request)
  (with-router router
    (handler-case (call-next-route)
      (route-not-found ()
        nil))))

(defmacro with-router-routes (&body body)
  `(let ((*routes* (routes *router*)))
     ,@body))

(defun spare-routes ()
  "Список доступных маршрутов из текущего положения"
  (iter (for route in (routes *router*))
        (unless (route? route)
          (collect route))))
