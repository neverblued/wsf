(in-package #:wsf)

(defgeneric respond (site request))

(defgeneric default-response (site request))

(defmethod respond :around (site request)
  (let (*response*)
    (declare (special *response*))
    (kgb::with-authentication request
      (call-next-method))
    (send (if (typep *response* 'response)
              *response*
              (default-response site request)))))

(defmethod respond (site request)
  (declare (special *response*))
  (route site request))

(defmethod default-response (site request)
  (declare (ignore site request))
  (make-instance 'text-response
                 :content (join "Unfortunately, the website has no"
                                " appropriate content for your request"
                                " (404)."
                                )
                 ))
