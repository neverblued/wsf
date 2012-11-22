;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defun uri-http-request (uri)
  (make-instance 'request
                 :acceptor *acceptor*
                 :server-protocol :HTTP/1.1
                 :method :get
                 :remote-addr "0.0.0.0"
                 :headers-in nil
                 :uri uri))

(defclass headless-reply (reply) ())

(defmacro with-headless-reply (&body body)
  `(let ((*reply* (make-instance 'headless-reply)))
     ,@body))

(defun within-headless-reply? ()
  (typep *reply* 'headless-reply))

(defun within-http-request? ()
  (and (within-request-p)
       (boundp '*reply*)))

(defmethod send :before (response)
  (if (within-headless-reply?)
      (format t "~&WSF headless response # ~a~&~%" (status response))
      (set-reply response)))
