;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defgeneric server-domain (server))
(defgeneric server-port (server))
(defgeneric server-acceptor (server))

(defclass http-server ()
  ((domain :initarg :domain :accessor server-domain :initform nil)
   (port :initarg :port :accessor server-port :initform default-port)
   (acceptor :accessor server-acceptor)))

(defmethod start ((server http-server))
  (start (server-acceptor server)))

(defmethod stop ((server http-server) &key soft)
  (declare (ignore soft))
  (stop (server-acceptor server)))

(defun online? (http-server)
  (accepting? (server-acceptor http-server)))

(defun fetch-acceptor (http-server)
  (with-accessors ((port server-port)
                   (domain server-domain))
      http-server
    (or (let ((result (acceptors :domain domain :port port)))
          (awhen (first result) it))
        (make-instance 'acceptor
                       :server http-server
                       :address domain
                       :port port))))

(defmethod initialize-instance :after ((server http-server) &key)
  (setf (server-acceptor server)
        (fetch-acceptor server))
  ;(start server) @fixme
  t)

(defmacro with-http-acceptor (&body body)
  `(let ((*acceptor* (server-acceptor *server*)))
     ,@body))
