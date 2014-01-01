;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

;; common

(define-condition wsf-condition () ())

;; server

(define-condition server-condition (wsf-condition)
  ((server :initarg :server :reader condition-server)))

(define-condition server-on (server-condition simple-warning) ())

(defmethod print-object ((condition server-on) stream)
  (format stream "~&Сайт ~a в эфире.~%" (condition-server condition)))

(define-condition server-off (server-condition simple-warning) ())

(defmethod print-object ((condition server-off) stream)
  (format stream "~&Сайт ~a больше не доступен.~%" (condition-server condition)))

;; route

(define-condition no-response (wsf-condition)
  ((router :initarg :router :reader condition-router)
   (route :initarg :route :reader condition-route)
   (request :initarg :request :reader condition-request)))

(define-condition route-not-found (wsf-condition error)
  ((router :initarg :router :reader condition-router :initform *server*)
   (request :initarg :request :reader condition-request :initform *request*)))

(define-condition invalid-action (wsf-condition type-error)
  ((route :initarg :route :reader condition-route)))
