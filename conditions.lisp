(in-package #:wsf)

(define-condition wsf-condition () ())

(define-condition unacceptable-host (wsf-condition)
  ((host :initarg :host :reader rejected-host)))
