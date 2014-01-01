;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defclass router ()
  ((routes :accessor routes :initform nil)))

(defmethod respond ((router router) request)
  (handler-case (call-next-route)
    (route-not-found ()
      nil)))

(defmacro with-server-routes (&body body)
  `(let ((*routes* (routes *server*)))
     ,@body))

(defun spare-routes ()
  "Список доступных маршрутов из текущего положения"
  (iter (for route in (routes *server*))
        (unless (route? route)
          (collect route))))
