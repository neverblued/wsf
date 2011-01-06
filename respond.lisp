(in-package #:wsf)

(defgeneric respond (site request))

(defgeneric default-response (site request))

(defgeneric error-response (site request))

(defmethod respond :around (site request)
  (let (*response*)
    (declare (special *response*))
    (kgb::with-authentication request
      (call-next-method))
    (send (if (typep *response* 'response)
              *response*
              (default-response site request)))))

(defmethod respond (site (request request))
  (handler-case (route site request)
    (error (condition)
      (declare (special *response*))
      (setf *response* (error-response site condition)))))

(defmethod respond (site (uri string))
  (let ((*acceptor* (site-acceptor site))
        (*reply* (make-instance 'reply)))
    (route site (make-instance 'request :uri uri :acceptor *acceptor*))))

(defmethod default-response (site request)
  (declare (ignore site request))
  (make-instance 'text-response
                 :content (join "Unfortunately, the website has no"
                                " appropriate content for your request"
                                " (404)."
                                )
                 ))

(defmethod error-response (site condition)
  (declare (ignore site))
  (make-instance 'html-response
                 :title "Server error"
                 :content (format nil "<h1>Server error</h1><p>~a</p>" condition)
                 ))
