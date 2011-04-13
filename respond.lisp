(in-package #:wsf)

(defgeneric respond (site request))
(defgeneric default-response (site request))
(defgeneric failure-response (site request condition))

(defvar *site*)
(defvar *response*)

(defmethod respond :around (site request)
  (let ((*site* site) (*request* request) *response*)
    (kgb::with-authentication request
      (catch 'response (call-next-method)))
    (send (typecase *response*
            (response *response*)
            (t (default-response site request))))))

(defun throw-response (response)
  (setf *response* response)
  (throw 'response nil))

(defmethod respond (site (request request))
  (handler-case (route site request)
    ((or warning error)
        (condition)
      (throw-response (failure-response site request condition)))))

(defmethod respond (site (uri string))
  (let ((*acceptor* (site-acceptor site))
        (*reply* (make-instance 'reply)))
    (route site (make-instance 'request :uri uri :acceptor *acceptor*))))

(defmethod default-response (site request)
  (declare (ignore site request))
  (make-instance 'text-response
                 :content "Сервер не знает, как ответить (ошибка 404)."))

(defmethod failure-response (site request condition)
  (declare (ignore site request))
  (make-instance 'html-response
                 :title "Отказ сервера"
                 :content (format nil "<h1>Отказ сервера</h1><p>~a</p><p>:( <big>&rarr;</big> <a href='/'>:)</a></p>" condition)))
