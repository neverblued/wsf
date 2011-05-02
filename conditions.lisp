(in-package #:wsf)

;; common

(define-condition wsf-condition () ())

;; site

(define-condition site-condition (wsf-condition)
  ((site :initarg :site :reader condition-site)))

(define-condition site-on (site-condition simple-warning) ())

(defmethod print-object ((condition site-on) stream)
  (format stream "~&Сайт ~a в эфире.~%" (condition-site condition)))

(define-condition site-off (site-condition simple-warning) ())

(defmethod print-object ((condition site-off) stream)
  (format stream "~&Сайт ~a больше не доступен.~%" (condition-site condition)))

;; route

(define-condition route-not-found (wsf-condition)
  ((controller :initarg :controller :reader condition-controller)
   (request :initarg :request :reader condition-request)))

(define-condition invalid-action (wsf-condition type-error)
  ((route :initarg :route :reader condition-route)))

(define-condition undefined-ajax-action (error)
  ((action-name :initarg :action-name :reader condition-action-name))
  (:report (lambda (condition stream)
             (format stream "Неизвестное действие ~a." (condition-action-name condition)))))
