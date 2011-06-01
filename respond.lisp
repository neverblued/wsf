(in-package #:wsf)

(defgeneric respond (site request))
(defgeneric default-response (site request))
(defgeneric failure-response (site request condition))

(defvar *site*)

(defmethod respond :around (site request)
  (let ((*site* site)
        (*request* request))
    (let ((response (catch 'response
                      (with-site-db site
                        (with-router site
                          (kgb::with-authentication request
                            (call-next-method)))))))
      (send (typecase response
              (response response)
              (string (make-instance 'text-response
                                     :content (join "Ответ сервера: " response)))
              (t (default-response site request)))))))

(defun throw-response (response)
  (throw 'response response))

(defparameter *catch-errors* t)

(defmethod respond (site (request request))
  (handler-case (route!)
    ((or warning error wsf-condition) (condition)
      (if (and *catch-errors* (not (boundp '*reply*)))
          (invoke-debugger condition)
          (throw-response (failure-response site request condition))))))

(defmethod respond (site (uri string))
  (let* ((*acceptor* (site-acceptor site))
         (*reply* (make-instance 'reply))
         (*request* (make-instance 'request :uri uri :acceptor *acceptor*)))
    (route!)))

(defun dongle-message (title message)
  (format nil "<center><h1>~a</h1><big><p>~a</p><p>:( <big>&rarr;</big> <a href='/'>:)</a></p></big><center>" title message))

(defmethod default-response (site request)
  (declare (ignore request))
  (make-instance 'html-response
                 :status +http-not-found+
                 :content (dongle-message "Ошибка 404" "Сервер не знает, как ответить.")))

(defmethod failure-response (site request condition)
  (declare (ignore request))
  (make-instance 'html-response
                 :status +http-internal-server-error+
                 :title "Отказ сервера"
                 :content (dongle-message "Отказ сервера" condition)))
