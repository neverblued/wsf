;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defparameter *charsets* `(
                           (:utf-8 (,hunchentoot::+utf-8+ "utf-8"))
                           ))

(flet ((? (charset)
         (find-assoc charset *charsets* :test #'eql)))

  (defun charset-instance (charset)
    (first (? charset)))

  (defun charset-string (charset)
    (second (? charset))))
