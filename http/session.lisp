;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defmacro with-http-session (&body body)
  `(if (boundp '*session*)
       (progn ,@body)
       (let ((*session* (make-instance 'session)))
         ,@body)))

(defun assert-session ()
  (unless (boundp '*session*)
    (start-session)))
