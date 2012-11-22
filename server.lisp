;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defgeneric respond (server request))
(defgeneric default-response (server request))
(defgeneric failure-response (server request condition))

(defmethod respond (server request)
  (default-response server request))

(defvar *server*)

(defmacro with-server (server &body body)
  `(let ((*server* ,server))
     ,@body))

(defmacro with-server-request ((server request) &body body)
  `(let* ((*server* ,server)
          (*request* ,request))
     ,@body))

(defmethod respond :around (server request)
  (with-server-request (server request)
    (call-next-method)))
