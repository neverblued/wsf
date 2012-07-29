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
  (start server))

(defun throw-response (response)
  (throw 'response response))

(defmethod respond :around ((server http-server) (uri string))
  (let* ((*acceptor* (server-acceptor server))
         (*reply* (make-instance 'reply))
         (*request* (make-instance 'request
                                   :acceptor *acceptor*
                                   :server-protocol :HTTP/1.1
                                   :method :get
                                   :remote-addr "0.0.0.0"
                                   :headers-in nil
                                   :uri uri))
         (*session* (make-instance 'session))
         (hunchentoot::*hunchentoot-stream* *debug-io*))
    ;(setf (header-out "request-source" *request*) "string")
    (respond server *request*)))

(defun assert-session ()
  (unless (boundp '*session*)
    (start-session)))

(defmacro with-slime-debug (&body body)
  `(handler-case (progn ,@body)
     ((or warning error wsf-condition)
         (condition)
       (if (and slime-debug-conditions
                (not (boundp '*reply*)))
           (invoke-debugger condition)
           (throw-response (failure-response server request condition))))))

(defmethod respond :around ((server http-server) (request request))
  (awith (catch 'response
           (with-slime-debug
             (call-next-method)))
    (send (typecase it
            (response it)
            (string (make-instance 'text-response
                                   :content (join "Сервер ответил строкою: " it)))
            (t (default-response server request))))))

(labels ((content (title message)
           (format nil default-http-content-format title message))

         (response (status title description)
           (make-instance 'html-response
                          :status status
                          :title title
                          :content (content title description))))

  (defmethod default-response ((server http-server) request)
    (response +http-not-found+
              "Страница не найдена"
              "Сервер не знает, как ответить."))

  (defmethod failure-response ((server http-server) request condition)
    (response +http-internal-server-error+
              "Отказ сервера"
              condition)))
