;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defclass website (http-server docroot-server parse-server router) ())

(defparameter server-charset :utf-8)

(defmethod respond :around ((server website) request)
  (let ((*hunchentoot-default-external-format* (charset-instance server-charset)))
    (call-next-method)))
