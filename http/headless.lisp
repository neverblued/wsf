;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defun uri-http-request (uri)
  (make-instance 'request
                 :acceptor *acceptor*
                 :content-stream *debug-io*
                 :server-protocol :HTTP/1.1
                 :method :get
                 :remote-addr "0.0.0.0"
                 :headers-in nil
                 :uri uri))

(defclass headless-reply (reply) ())

(defun within-headless-reply? ()
  (and (boundp '*reply*)
       (typep *reply* 'headless-reply)))

(defmacro with-headless-reply (&body body)
  `(let ((*reply* (make-instance 'headless-reply)))
     ,@body))
