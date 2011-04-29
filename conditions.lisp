(in-package #:wsf)

(define-condition wsf-condition () ())

(define-condition site-condition (wsf-condition)
  ((site :initarg :site :reader condition-site)))

(define-condition site-on (site-condition simple-warning) ())

(defmethod print-object ((condition site-on) stream)
  (format stream "~&Сайт ~a в эфире.~%" (condition-site condition)))

(define-condition site-off (site-condition simple-warning) ())

(defmethod print-object ((condition site-off) stream)
  (format stream "~&Сайт ~a больше не доступен.~%" (condition-site condition)))
