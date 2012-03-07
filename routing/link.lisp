;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defgeneric make-link (router route-key &rest args)
  (:documentation "Собрать ссылку"))

(defun link (route-key &rest parameters)
  (apply #'make-link *router* route-key parameters))

(defparameter broken-link "/404-broken-link/")

(defparameter broken-link? nil)

(defun broken-link ()
  (if broken-link? broken-link
      (let ((broken-link? t))
        (aif (find-default-route)
             (link it)
             (broken-link)))))

(defmethod make-link (router route &rest parameters)
  (with-router router
    (aif (with-router-routes (route! route))
         (let ((link (apply (route-link it) parameters)))
           (aif (pookies)
                (insert-get-parameters link it)
                link))
         (broken-link))))
