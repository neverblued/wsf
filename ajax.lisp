(in-package #:wsf)

(defgeneric ajax-actions (site))

(defgeneric ajax-action (site action-name)
  (:documentation "Site AJAX action (by name)."))

(macrolet ((action (site action-name)
             `(gethash ,action-name (ajax-actions ,site))))

  (defmethod ajax-action (site action-name)
    (action site action-name))

  (defmethod (setf ajax-action) (new-action site action-name)
    (setf (action site action-name) new-action)))

(defmacro set-ajax (site action-name (&rest action-args) &body action-body)
  `(setf (ajax-action ,site ,action-name)
         (lambda (&key ,@action-args)
           (declare (ignorable ,@action-args))
           ,@action-body)))

(defgeneric ajax-response (site action-name action-args))

(defmethod ajax-response (site action-name action-args)
  (let ((action (ajax-action site action-name)))
    (if (functionp action)
        (apply action action-args)
        (list :ajax-error (list "unknown_action_name" action-name)))))

(defmethod ajax-response :around (site action-name action-args)
  (jsun::encode (call-next-method)))

(defmacro set-ajax-route (site)
  `(set-route ,site :ajax
              :args (action-name action-args)
              :link (join "/ajax/" action-name)
              :clause (with-request request-uri begins-with? "/ajax/")
              :params (list :action-name (keyword<-string (trim-left "/ajax/" (request-uri wsf::*request*)))
                            :action-args (loop for prm in (post-parameters wsf::*request*)
                                            collect (keyword<-string (car prm))
                                            collect (cdr prm)))
              :action (setf *response* (make-instance 'text-response :content (ajax-response *site* action-name action-args)))
              ))
