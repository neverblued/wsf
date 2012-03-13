;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defvar *route* nil "Текущий маршрут")

(defmacro with-route (route &body body)
  `(let* ((*route* ,route)
          (*routes* (adjoin *route* *routes*)))
     ,@body))

(defun call-route (route)
  (with-route route
    (funcall (route-action route))))

(defun may-follow? (route)
  (route? (route-follow route)))

(defun clause-fits? (route)
  (funcall (route-clause route)))

(defun find-next-route ()
  (iter (for route in (spare-routes))
        (when (and (may-follow? route)
                   (clause-fits? route))
          (leave route))))

(defun find-default-route ()
  (with-server-routes (or (route :default)
                          (signal 'route-not-found))))

(defun call-next-route ()
  (call-route (or (find-next-route)
                  (find-default-route))))
